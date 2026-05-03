;;; ellie.el --- Emacs client for ollie-9p  -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, chat

;;; Commentary:
;;
;; ollie.el talks to an ollie-9p session through plain file I/O on the
;; mounted filesystem (default: ~/mnt/ollie, or $OLLIE).
;;
;; Quick start:
;;
;;   (require 'ellie)        ; or use use-package / :load-path
;;   M-x ellie              ; opens *ellie*, attaches to last session or creates one
;;
;; In the *ellie* buffer:
;;
;;   RET       open an input window to compose and submit a prompt
;;   C-c C-q   open an input window to compose a queued prompt
;;   C-c C-c   stop the running turn
;;   C-c C-k   kill the current session
;;   C-c C-n   create a new session (also bound to "n" when idle)
;;   C-c C-a   attach to a different existing session
;;   C-c C-p   show the rendered system prompt
;;   g         force-refresh the chat log
;;
;; In the *ellie-input* window:
;;
;;   C-c C-c   submit
;;   C-c C-k   cancel
;;
;; State changes while the agent is running:
;;
;;   Writes to cfg (backend=, model=, agent=) are rejected by the
;;   server when the agent is not idle.  Commands sent via ctl (e.g.
;;   compact, clear) are dispatched asynchronously and cannot return
;;   errors; if the agent is running, the command is silently rejected.
;;   Read cfg and check state= to confirm a change took effect.

;;; Code:

(require 'ansi-color)
(require 'markdown-mode nil t)

;;;; ──────────────── Customization ────────────────

(defgroup ellie nil
  "Emacs client for ollie-9p."
  :group 'tools
  :prefix "ellie-")

(defcustom ellie-mount-directory
  (or (getenv "OLLIE") (expand-file-name "~/mnt/ollie"))
  "Mount point for the ollie-9p filesystem."
  :type 'directory
  :group 'ellie)

(defcustom ellie-default-session-opts nil
  "Default options passed when creating a new session.
An alist of (KEY . VALUE) string pairs; valid keys are
\"backend\", \"model\", \"agent\", and \"cwd\"."
  :type '(alist :key-type string :value-type string)
  :group 'ellie)

(defcustom ellie-poll-interval 0.5
  "Seconds between chat-file polls (used when file-notify is unavailable)."
  :type 'number
  :group 'ellie)

(defcustom ellie-input-window-height 8
  "Height in lines of the prompt input window."
  :type 'integer
  :group 'ellie)

;;;; ──────────────── Internal state ────────────────

(defvar ellie--session-id nil
  "ID of the currently active session (global).")

(defvar ellie--chat-size -1
  "Last known byte count of the chat file; -1 means unread.")

(defvar ellie--poll-timer nil
  "Timer for periodic refresh.")

(defconst ellie--buf   "*ellie*"       "Name of the chat display buffer.")
(defconst ellie--ibuf  "*ellie-input*" "Name of the prompt input buffer.")

;;;; ──────────────── Paths ────────────────

(defun ellie--sessions-dir ()
  (expand-file-name "s" ellie-mount-directory))

(defun ellie--sessions-new-path ()
  (expand-file-name "new" (ellie--sessions-dir)))

(defun ellie--session-file (name)
  "Return the full path to file NAME inside the current session directory.
Signals `user-error' if no session is active."
  (unless ellie--session-id
    (user-error "No active ollie session"))
  (expand-file-name name
                    (expand-file-name ellie--session-id (ellie--sessions-dir))))

(defun ellie--last-session-path ()
  (expand-file-name "~/.config/ollie/last-session"))

;;;; ──────────────── Low-level file helpers ────────────────

(defun ellie--fwrite (path text)
  "Write TEXT to PATH, suppressing the minibuffer message."
  (write-region text nil path nil 'silent))

(defun ellie--fread (path)
  "Return the contents of PATH as a string, or nil if it does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))


;;;; ──────────────── Session enumeration ────────────────

(defun ellie--session-ids ()
  "Return a list of all current session IDs."
  (let ((sdir (ellie--sessions-dir)))
    (when (file-directory-p sdir)
      (seq-filter
       (lambda (name) (not (string-prefix-p "." name)))
       (mapcar #'file-name-nondirectory
               (seq-filter #'file-directory-p
                           (directory-files sdir t)))))))

;;;; ──────────────── Last-session persistence ────────────────

(defun ellie--persist-session (id)
  "Upsert the {cwd}TAB{id} entry for `default-directory' in the last-session file."
  (let* ((path (ellie--last-session-path))
         (cwd  (expand-file-name default-directory))
         (raw  (ellie--fread path))
         (lines (when raw
                  (seq-filter (lambda (l) (not (string-empty-p (string-trim l))))
                              (split-string raw "\n"))))
         (entry (concat cwd "\t" id))
         (found nil)
         (new-lines
          (mapcar (lambda (l)
                    (let ((parts (split-string l "\t" nil nil)))
                      (if (and (= (length parts) 2) (string= (car parts) cwd))
                          (progn (setq found t) entry)
                        l)))
                  lines)))
    (make-directory (file-name-directory path) t)
    (ellie--fwrite path
                   (concat (mapconcat #'identity
                                      (if found new-lines (append new-lines (list entry)))
                                      "\n")
                           "\n"))))

(defun ellie--recall-session ()
  "Return the last-used session ID for `default-directory' if it still exists, else nil."
  (let* ((path (ellie--last-session-path))
         (cwd  (expand-file-name default-directory))
         (raw  (ellie--fread path)))
    (when raw
      (catch 'found
        (dolist (line (split-string raw "\n"))
          (let ((parts (split-string line "\t" nil nil)))
            (when (and (= (length parts) 2)
                       (string= (car parts) cwd))
              (let ((id (string-trim (cadr parts))))
                (when (and (not (string-empty-p id))
                           (file-directory-p (expand-file-name id (ellie--sessions-dir))))
                  (throw 'found id))))))))))

;;;; ──────────────── Session lifecycle ────────────────

(defun ellie-new-session (&optional opts)
  "Create a new ollie session and make it current.
OPTS is an alist of option pairs; defaults to `ellie-default-session-opts'.
Returns the new session ID."
  (interactive)
  (unless (file-directory-p ellie-mount-directory)
    (user-error "ollie-9p not mounted at %s (customize `ellie-mount-directory')"
                ellie-mount-directory))
  (let* ((effective-opts (or opts ellie-default-session-opts))
         ;; cwd is required; inject it if not already present.
         (effective-opts (if (assoc "cwd" effective-opts)
                             effective-opts
                           (cons (cons "cwd" default-directory) effective-opts)))
         (before      (ellie--session-ids))
         (seen        (make-hash-table :test 'equal))
         (cmd         (mapconcat (lambda (kv)
                                   (format "%s=%s" (car kv) (cdr kv)))
                                 effective-opts " ")))
    (dolist (id before) (puthash id t seen))
    (ellie--fwrite (ellie--sessions-new-path) (concat cmd "\n"))
    (let ((deadline (+ (float-time) 5.0))
          result)
      (while (and (not result) (< (float-time) deadline))
        (dolist (id (ellie--session-ids))
          (unless (gethash id seen) (setq result id)))
        (unless result (sleep-for 0.05)))
      (unless result
        (user-error "Timed out waiting for a new ollie session"))
      (setq ellie--session-id result)
      (ellie--persist-session result)
      (message "Created ollie session %s" result)
      result)))

(defun ellie-attach (id)
  "Attach to an existing session by ID and make it current."
  (interactive
   (let ((ids (ellie--session-ids)))
     (unless ids (user-error "No ollie sessions available"))
     (list (completing-read "Attach to session: " ids nil t))))
  (unless (file-directory-p (expand-file-name id (ellie--sessions-dir)))
    (user-error "Session not found: %s" id))
  (setq ellie--session-id id)
  (ellie--persist-session id)
  (message "Attached to ollie session %s" id))

(defun ellie-kill-session ()
  "Kill the current ollie session."
  (interactive)
  (unless ellie--session-id (user-error "No active session"))
  (when (yes-or-no-p (format "Kill ollie session %s? " ellie--session-id))
    (delete-directory (expand-file-name ellie--session-id (ellie--sessions-dir)) t)
    (ellie--stop-watching)
    (setq ellie--session-id nil
          ellie--chat-size -1)
    (when-let (buf (get-buffer ellie--buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "(session killed)\n\n")
          (insert "Press 'n' or C-c C-n to start a new session.\n"))))
    (message "Session killed")))

(defun ellie-rename-session (new-name)
  "Rename the current ollie session to NEW-NAME."
  (interactive "sNew session name: ")
  (unless ellie--session-id (user-error "No active session"))
  (when (string-empty-p (string-trim new-name))
    (user-error "Empty name"))
  (let ((old-dir (expand-file-name ellie--session-id (ellie--sessions-dir)))
        (new-dir (expand-file-name new-name (ellie--sessions-dir))))
    (rename-file old-dir new-dir)
    (setq ellie--session-id new-name)
    (ellie--persist-session new-name)
    (force-mode-line-update)
    (message "Renamed session to %s" new-name)))

;;;; ──────────────── State ────────────────

(defun ellie--state ()
  "Return the current session state string, or \"unknown\" on any error."
  (condition-case nil
      (let ((spec (ellie--fread (ellie--session-file "cfg"))))
        (if (and spec (string-match "^state=\\(.*\\)$" spec))
            (string-trim (match-string 1 spec))
          "unknown"))
    (error "unknown")))

(defun ellie--idle-p ()
  "Return non-nil if the session is currently idle."
  (ignore-errors (string= (ellie--state) "idle")))

;;;; ──────────────── Agent operations ────────────────

(defun ellie-submit (text)
  "Submit TEXT as a prompt to the current session.
The server dispatches it asynchronously."
  (when (string-empty-p (string-trim text))
    (user-error "Empty prompt"))
  (ellie--fwrite (ellie--session-file "prompt") text))

(defun ellie-queue (text)
  "Enqueue TEXT as a prompt to run after the current turn finishes."
  (when (string-empty-p (string-trim text))
    (user-error "Empty prompt"))
  (ellie--fwrite (ellie--session-file "fifo.in") text))

(defun ellie-stop ()
  "Send a stop signal to the current turn."
  (interactive)
  (ellie--fwrite (ellie--session-file "ctl") "stop\n")
  (message "ollie: stopped"))

;;;; ──────────────── Chat display ────────────────

;; We cache the mtime so we only re-read the file when it actually changes.
;; The mode-line state is updated on every refresh tick.

(defvar-local ellie--mode-line-state "unknown"
  "Cached session state string for the mode line.")

(defvar-local ellie--mode-line-usage ""
  "Cached usage string for the mode line.")



(defun ellie--refresh ()
  "Refresh the chat buffer if the chat file has grown on disk.
Also updates the cached mode-line state."
  (let* ((buf   (get-buffer ellie--buf))
         (chat  (condition-case nil (ellie--session-file "chat") (error nil)))
         (attrs (and chat (file-attributes chat)))
         (size  (and attrs (file-attribute-size attrs))))
    (when buf
      (with-current-buffer buf
        (setq ellie--mode-line-state (ellie--state))
        (setq ellie--mode-line-usage
              (or (condition-case nil
                      (when-let (s (ellie--fread (ellie--session-file "ctxsz")))
                        (string-trim s))
                    (error nil))
                  ""))
        ;; The chat log is append-only, so size increasing means new content.
        (when (and size (> size ellie--chat-size))
          (setq ellie--chat-size size)
          (let ((inhibit-read-only t)
                (at-end (= (point) (point-max))))
            (erase-buffer)
            (insert-file-contents chat)
            (ansi-color-apply-on-region (point-min) (point-max))
            (font-lock-ensure)
            (when at-end (goto-char (point-max)))))
        (force-mode-line-update)))))

;;;; ──────────────── Polling ────────────────

;; inotify does not fire on 9P mounts, so we always use a timer.
;; The mtime check in `ellie--refresh' keeps this cheap.

(defun ellie--start-watching ()
  "Start a periodic timer to tail the chat file."
  (ellie--stop-watching)
  (setq ellie--poll-timer
        (run-with-timer ellie-poll-interval ellie-poll-interval #'ellie--refresh)))

(defun ellie--stop-watching ()
  "Cancel the refresh timer."
  (when ellie--poll-timer
    (cancel-timer ellie--poll-timer)
    (setq ellie--poll-timer nil)))

;;;; ──────────────── Input buffer ────────────────

(defvar-local ellie--input-queued nil
  "Non-nil when this input buffer will enqueue rather than submit.")

(defvar ellie-input-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'ellie-input-submit)
    (define-key m (kbd "C-c C-k") #'ellie-input-cancel)
    m)
  "Keymap for `ellie-input-mode'.")

(define-derived-mode ellie-input-mode text-mode "Ollie-Input"
  "Major mode for composing ollie prompts.
\\{ellie-input-mode-map}"
  (setq-local header-line-format
              '(:eval
                (format "  %s  ·  C-c C-c submit  ·  C-c C-k cancel"
                        (if ellie--input-queued
                            (propertize "Queue prompt" 'face 'warning)
                          (propertize "Send prompt"  'face 'success))))))

(defun ellie--open-input (&optional queued)
  "Open the prompt input window below the current window.
QUEUED non-nil means the prompt will be enqueued, not submitted immediately."
  (let ((buf (get-buffer-create ellie--ibuf)))
    (with-current-buffer buf
      (erase-buffer)
      (ellie-input-mode)
      (setq ellie--input-queued queued))
    (display-buffer buf
                    `(display-buffer-below-selected
                      (window-height . ,ellie-input-window-height)))
    (when-let (win (get-buffer-window buf t))
      (select-window win))))

(defun ellie-input-submit ()
  "Submit the contents of the input buffer as a prompt."
  (interactive)
  (let ((text   (buffer-string))
        (queued ellie--input-queued))
    (ellie--close-input-window)
    (if queued
        (ellie-queue text)
      (ellie-submit text))))

(defun ellie-input-cancel ()
  "Discard the input and close the input window."
  (interactive)
  (ellie--close-input-window))

(defun ellie--close-input-window ()
  "Kill the input buffer and delete its window."
  (when-let (win (get-buffer-window ellie--ibuf t))
    (delete-window win))
  (when-let (buf (get-buffer ellie--ibuf))
    (kill-buffer buf)))

;;;; ──────────────── Chat major mode ────────────────

(defvar ellie-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")     #'ellie-send-input)
    (define-key m (kbd "C-c C-q") #'ellie-queue-input)
    (define-key m (kbd "C-c C-c") #'ellie-stop)
    (define-key m (kbd "C-c C-k") #'ellie-kill-session)
    (define-key m (kbd "C-c C-r") #'ellie-rename-session)
    (define-key m (kbd "C-c C-n") #'ellie-new-and-open)
    (define-key m (kbd "C-c C-a") #'ellie-attach-and-open)
    (define-key m (kbd "C-c C-d") #'ellie-dired)
    (define-key m (kbd "C-c C-p") #'ellie-show-system-prompt)
    (define-key m (kbd "n")       #'ellie-new-and-open)
    (define-key m (kbd "g")       #'ellie-refresh)
    m)
  "Keymap for `ellie-mode'.")

(defun ellie-send-input ()
  "Open the input window to compose and submit a prompt."
  (interactive)
  (ellie--open-input nil))

(defun ellie-queue-input ()
  "Open the input window to compose a queued prompt."
  (interactive)
  (ellie--open-input t))

(defun ellie-refresh ()
  "Force a refresh of the chat log from disk."
  (interactive)
  (setq ellie--chat-size -1)
  (ellie--refresh))

(defun ellie-dired ()
  "Open the ollie-9p mount point in Dired."
  (interactive)
  (dired ellie-mount-directory))

(define-derived-mode ellie-mode special-mode "Ollie"
  "Major mode for the ollie chat interface.

Key bindings:
\\{ellie-mode-map}"
  :group 'ellie
  (buffer-disable-undo)
  (when (fboundp 'markdown-mode)
    (setq-local font-lock-defaults '(markdown-mode-font-lock-keywords t))
    (font-lock-mode 1))
  (setq-local mode-line-format
              `("%e"
                mode-line-front-space
                (:eval
                 (propertize (format " ellie:%s" (or ellie--session-id "-"))
                             'face 'mode-line-buffer-id))
                (:eval
                 (if (string= ellie--mode-line-state "idle")
                     (propertize "idle" 'face 'success)
                   (propertize ellie--mode-line-state 'face 'warning)))
                (:eval
                 (when (not (string-empty-p ellie--mode-line-usage))
                   (concat "  " (replace-regexp-in-string "%" "%%" ellie--mode-line-usage))))
                "  "
                mode-line-end-spaces)))

;;;; ──────────────── System prompt ────────────────

(defun ellie-show-system-prompt ()
  "Display the rendered system prompt for the current session in a buffer."
  (interactive)
  (let* ((path (ellie--session-file "systemprompt"))
         (text (ellie--fread path)))
    (unless text
      (user-error "Could not read system prompt"))
    (let ((buf (get-buffer-create "*ellie-system-prompt*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min)))
        (view-mode 1)
        (when (fboundp 'markdown-mode)
          (markdown-mode)))
      (pop-to-buffer buf))))

;;;; ──────────────── Entry points ────────────────

;;;###autoload
(defun ellie ()
  "Open the ollie chat interface.
Attaches to the last session if it still exists; otherwise creates a new one."
  (interactive)
  (unless (file-directory-p ellie-mount-directory)
    (user-error "ollie-9p not mounted at %s  (customize `ellie-mount-directory')"
                ellie-mount-directory))
  (unless ellie--session-id
    (if-let (last (ellie--recall-session))
        (progn
          (setq ellie--session-id last)
          (message "Attached to last ollie session %s" last))
      (ellie-new-session)))
  (ellie--open-buf))

;;;###autoload
(defun ellie-new-and-open ()
  "Create a new ollie session and open the chat buffer."
  (interactive)
  (ellie-new-session)
  (ellie--open-buf))

;;;###autoload
(defun ellie-attach-and-open (id)
  "Attach to session ID and open the chat buffer."
  (interactive
   (let ((ids (ellie--session-ids)))
     (unless ids (user-error "No ollie sessions available"))
     (list (completing-read "Session: " ids nil t))))
  (ellie-attach id)
  (ellie--open-buf))

(defun ellie--open-buf ()
  "Open or switch to the *ollie* chat buffer."
  (let ((buf (get-buffer-create ellie--buf)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ellie-mode)
        (ellie-mode))
      (setq ellie--chat-size -1)
      (ellie--refresh)
      (ellie--start-watching))
    (pop-to-buffer buf)))

;;;; ──────────────── Code completion ────────────────

(defcustom ellie-complete-backend
  (or (getenv "OLLIE_COMPLETE_BACKEND") "")
  "Backend for code completion (e.g. \"ollama\", \"openrouter\")."
  :type 'string
  :group 'ellie)

(defcustom ellie-complete-model
  (or (getenv "OLLIE_COMPLETE_MODEL") "")
  "Model for code completion (e.g. \"qwen3:4b\")."
  :type 'string
  :group 'ellie)

(defcustom ellie-complete-prefix-max 4000
  "Maximum characters of context before point."
  :type 'integer
  :group 'ellie)

(defcustom ellie-complete-suffix-max 1000
  "Maximum characters of context after point."
  :type 'integer
  :group 'ellie)

(defvar ellie--complete-timer nil
  "Timer for polling completion result.")

(defun ellie--complete-session-id ()
  "Return the deterministic copilot session ID for `default-directory'."
  (let* ((cwd (expand-file-name default-directory))
         ;; Emulate POSIX cksum: use the same CRC but just take the numeric hash.
         ;; We shell out once to stay consistent with the acme script.
         (hash (string-trim
                (shell-command-to-string
                 (format "printf '%%s' %s | cksum | awk '{print $1}'"
                         (shell-quote-argument cwd))))))
    (concat hash "-copilot")))

(defun ellie--complete-ensure-session ()
  "Find or create the copilot session for `default-directory'.
Returns the session directory path."
  (when (string-empty-p ellie-complete-backend)
    (user-error "Set `ellie-complete-backend' or $OLLIE_COMPLETE_BACKEND"))
  (when (string-empty-p ellie-complete-model)
    (user-error "Set `ellie-complete-model' or $OLLIE_COMPLETE_MODEL"))
  (let* ((id (ellie--complete-session-id))
         (sdir (expand-file-name id (ellie--sessions-dir))))
    (if (file-directory-p sdir)
        ;; Check for failed state; kill and recreate if needed.
        (let ((state (ellie--complete-state sdir)))
          (when (or (string-prefix-p "failed" state)
                    (string-prefix-p "error" state))
            (ignore-errors (ellie--fwrite (expand-file-name "ctl" sdir) "kill\n"))
            (sleep-for 0.1)
            (ellie--complete-create-session id))
          sdir)
      (ellie--complete-create-session id)
      sdir)))

(defun ellie--complete-create-session (id)
  "Create a copilot session with ID."
  (let ((spec (format "name=%s\ncwd=%s\nagent=copilot\nbackend=%s\nmodel=%s\n"
                       id (expand-file-name default-directory)
                       ellie-complete-backend ellie-complete-model)))
    (ellie--fwrite (ellie--sessions-new-path) spec)
    (let ((sdir (expand-file-name id (ellie--sessions-dir)))
          (deadline (+ (float-time) 5.0)))
      (while (and (not (file-directory-p sdir))
                  (< (float-time) deadline))
        (sleep-for 0.05))
      (unless (file-directory-p sdir)
        (user-error "Timeout waiting for copilot session %s" id)))))

(defun ellie--complete-strip (text)
  "Strip markdown fences and info lines from TEXT."
  (let ((lines (split-string text "\n")))
    (setq lines (seq-remove (lambda (l)
                              (or (string-match-p "\\````" l)
                                  (string-match-p "\\`:: " l)))
                            lines))
    (string-join lines "\n")))

(defun ellie--complete-state (sdir)
  "Read the session state from cfg in SDIR."
  (let ((cfg (ellie--fread (expand-file-name "cfg" sdir))))
    (if (and cfg (string-match "^state=\\(.*\\)$" cfg))
        (string-trim (match-string 1 cfg))
      "unknown")))

(defun ellie--read-from-byte-offset (path offset)
  "Read PATH and return content starting at byte OFFSET as a string."
  (if (or (<= offset 0) (not (file-exists-p path)))
      (or (ellie--fread path) "")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (let ((raw (buffer-substring-no-properties
                  (min (1+ offset) (point-max))
                  (point-max))))
        (decode-coding-string raw 'utf-8)))))

;; Ghost text state.
(defvar ellie--ghost-overlay nil "Overlay showing the ghost completion.")
(defvar ellie--ghost-text nil "The pending ghost text string.")
(defvar ellie--ghost-pos nil "Buffer position where ghost text should be inserted.")
(defvar ellie--ghost-region nil "Cons (BEG . END) of region to replace, or nil.")

(defface ellie-ghost '((t :foreground "gray50" :slant italic))
  "Face for ghost-text completion suggestions."
  :group 'ellie)

(defvar ellie-ghost-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB") #'ellie--ghost-accept-cmd)
    (define-key m (kbd "<tab>") #'ellie--ghost-accept-cmd)
    (define-key m (kbd "RET") #'ellie--ghost-accept-cmd)
    (define-key m (kbd "<return>") #'ellie--ghost-accept-cmd)
    m)
  "Keymap active while ghost text is showing.")

(define-minor-mode ellie-ghost-mode
  "Minor mode active while an ellie ghost-text suggestion is visible."
  :lighter " Ghost"
  :keymap ellie-ghost-mode-map)

(defun ellie--ghost-show (text pos buf &optional region)
  "Display TEXT as ghost text at POS in BUF.
If REGION is a cons (BEG . END), the ghost replaces that region on accept."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (ellie--ghost-dismiss)
      (setq ellie--ghost-text text
            ellie--ghost-region region)
      (if region
          ;; Region mode: overlay the region with ghost text.
          (let ((ov (make-overlay (car region) (cdr region) buf)))
            (setq ellie--ghost-pos (car region)
                  ellie--ghost-overlay ov)
            (overlay-put ov 'display (propertize text 'face 'ellie-ghost))
            (overlay-put ov 'ellie-ghost t))
        ;; No region: show ghost on next line.
        (save-excursion
          (goto-char pos)
          (end-of-line)
          (setq ellie--ghost-pos (point))
          (setq ellie--ghost-overlay (make-overlay (point) (point) buf))
          (overlay-put ellie--ghost-overlay 'after-string
                       (propertize (concat "\n" text) 'face 'ellie-ghost))
          (overlay-put ellie--ghost-overlay 'ellie-ghost t)))
      (ellie-ghost-mode 1)
      (add-hook 'pre-command-hook #'ellie--ghost-pre-command nil t))))

(defun ellie--ghost-dismiss ()
  "Remove any active ghost text."
  (when ellie--ghost-overlay
    (delete-overlay ellie--ghost-overlay)
    (setq ellie--ghost-overlay nil))
  (setq ellie--ghost-text nil
        ellie--ghost-pos nil
        ellie--ghost-region nil)
  (ellie-ghost-mode -1)
  (remove-hook 'pre-command-hook #'ellie--ghost-pre-command t))

(defun ellie--ghost-accept ()
  "Insert the ghost text into the buffer."
  (when ellie--ghost-text
    (let ((text ellie--ghost-text)
          (pos ellie--ghost-pos)
          (region ellie--ghost-region))
      (ellie--ghost-dismiss)
      (if region
          (progn
            (delete-region (car region) (cdr region))
            (goto-char (car region))
            (insert text))
        (save-excursion
          (goto-char pos)
          (insert "\n" text))
        (goto-char (+ pos 1 (length text)))))))

(defun ellie--ghost-accept-cmd ()
  "Interactive command to accept ghost text."
  (interactive)
  (ellie--ghost-accept))

(defun ellie--ghost-pre-command ()
  "Dismiss ghost text on any command not handled by the ghost keymap."
  (unless (memq this-command '(ellie--ghost-accept-cmd ellie-complete))
    (ellie--ghost-dismiss)))

;;;###autoload
(defun ellie-complete ()
  "Request AI code completion and show it as ghost text on the next line.
\\`TAB' accepts the suggestion; any other key dismisses it.
Calling again while a suggestion is showing requests a new one."
  (interactive)
  (let* ((prev-region ellie--ghost-region))
    (ellie--ghost-dismiss)
    (let* ((sdir (ellie--complete-ensure-session))
           (filename (or (buffer-file-name) (buffer-name)))
           (has-region (or (use-region-p) prev-region))
           (reg-beg (cond ((use-region-p) (region-beginning))
                          (prev-region (car prev-region))))
           (reg-end (cond ((use-region-p) (region-end))
                          (prev-region (cdr prev-region))))
           (ctx-point (if has-region reg-beg (point)))
           (prefix (buffer-substring-no-properties
                    (max (point-min) (- ctx-point ellie-complete-prefix-max))
                    ctx-point))
           (selected (if has-region
                         (buffer-substring-no-properties reg-beg reg-end)
                       ""))
           (suffix (buffer-substring-no-properties
                    (if has-region reg-end (point))
                    (min (point-max) (+ (if has-region reg-end (point))
                                        ellie-complete-suffix-max))))
           (prompt (if has-region
                     (format "Replace the selected code in %s. Output ONLY the replacement text — no markdown fences, no explanation.\n\n<prefix>\n%s\n</prefix>\n<selected>\n%s\n</selected>\n<suffix>\n%s\n</suffix>"
                             (file-name-nondirectory filename) prefix selected suffix)
                   (format "Complete the code at the cursor in %s. Output ONLY the raw text to insert at the cursor position. Do NOT wrap the output in markdown code fences or backticks. No explanation, no preamble.\n\n<prefix>\n%s\n</prefix>\n<suffix>\n%s\n</suffix>"
                           (file-name-nondirectory filename) prefix suffix)))
         (cur-point (point))
         (region-cons (and has-region (cons reg-beg reg-end)))
         (buf (current-buffer)))
    (when has-region (deactivate-mark))
    (message "ellie: completing...")
    (ellie--fwrite (expand-file-name "prompt" sdir) prompt)
    (when ellie--complete-timer (cancel-timer ellie--complete-timer))
    (let ((deadline (+ (float-time) 30.0)))
      (setq ellie--complete-timer
            (run-with-timer 0.3 0.3
                            (lambda ()
                              (condition-case err
                                  (let ((state (ellie--complete-state sdir)))
                                    (cond
                                     ((string= state "idle")
                                      (cancel-timer ellie--complete-timer)
                                      (setq ellie--complete-timer nil)
                                      (let* ((offset (string-to-number
                                                      (or (ellie--fread
                                                           (expand-file-name "offset" sdir))
                                                          "0")))
                                             (chat-path (expand-file-name "chat" sdir))
                                             (result (string-trim-right
                                                      (ellie--complete-strip
                                                       (ellie--read-from-byte-offset chat-path offset)))))
                                        (ellie--fwrite (expand-file-name "ctl" sdir) "clear\n")
                                        (if (string-empty-p (string-trim result))
                                            (message "ellie: empty completion")
                                          (ellie--ghost-show result cur-point buf region-cons)
                                          (message "ellie: TAB/RET accept · M-/ retry · any key dismiss"))))
                                     ((> (float-time) deadline)
                                      (cancel-timer ellie--complete-timer)
                                      (setq ellie--complete-timer nil)
                                      (message "ellie: completion timed out"))))
                                (error
                                 (cancel-timer ellie--complete-timer)
                                 (setq ellie--complete-timer nil)
                                 (message "ellie: completion error: %s" err))))))))))

;;;###autoload
(define-key global-map (kbd "M-/") #'ellie-complete)

(provide 'ellie)
;;; ellie.el ends here
