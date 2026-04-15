;;; ollie.el --- Emacs client for ollie-9p  -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, chat

;;; Commentary:
;;
;; ollie.el talks to an ollie-9p session through plain file I/O on the
;; mounted filesystem (default: ~/mnt/ollie, or $OLLIE_9MOUNT).
;;
;; Quick start:
;;
;;   (require 'ollie)        ; or use use-package / :load-path
;;   M-x ollie              ; opens *ollie*, attaches to last session or creates one
;;
;; In the *ollie* buffer:
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
;; In the *ollie-input* window:
;;
;;   C-c C-c   submit
;;   C-c C-k   cancel
;;
;; State changes while the agent is running:
;;
;;   Writes to the backend, model, and agent files are rejected by the
;;   server when the agent is not idle.  Commands sent via ctl (e.g.
;;   compact, clear, model) are dispatched asynchronously and cannot
;;   return errors; if the agent is running, the command is silently
;;   rejected.  Read the state file to confirm a change took effect.

;;; Code:

(require 'ansi-color)

;;;; ──────────────── Customization ────────────────

(defgroup ollie nil
  "Emacs client for ollie-9p."
  :group 'tools
  :prefix "ollie-")

(defcustom ollie-mount-directory
  (or (getenv "OLLIE_9MOUNT") (expand-file-name "~/mnt/ollie"))
  "Mount point for the ollie-9p filesystem."
  :type 'directory
  :group 'ollie)

(defcustom ollie-default-session-opts nil
  "Default options passed when creating a new session.
An alist of (KEY . VALUE) string pairs; valid keys are
\"backend\", \"model\", \"agent\", and \"cwd\"."
  :type '(alist :key-type string :value-type string)
  :group 'ollie)

(defcustom ollie-poll-interval 0.5
  "Seconds between chat-file polls (used when file-notify is unavailable)."
  :type 'number
  :group 'ollie)

(defcustom ollie-input-window-height 8
  "Height in lines of the prompt input window."
  :type 'integer
  :group 'ollie)

;;;; ──────────────── Internal state ────────────────

(defvar ollie--session-id nil
  "ID of the currently active session (global).")

(defvar ollie--chat-size -1
  "Last known byte count of the chat file; -1 means unread.")

(defvar ollie--poll-timer nil
  "Timer for periodic refresh.")

(defconst ollie--buf   "*ollie*"       "Name of the chat display buffer.")
(defconst ollie--ibuf  "*ollie-input*" "Name of the prompt input buffer.")

;;;; ──────────────── Paths ────────────────

(defun ollie--sessions-dir ()
  (expand-file-name "s" ollie-mount-directory))

(defun ollie--sessions-new-path ()
  (expand-file-name "new" (ollie--sessions-dir)))

(defun ollie--session-file (name)
  "Return the full path to file NAME inside the current session directory.
Signals `user-error' if no session is active."
  (unless ollie--session-id
    (user-error "No active ollie session"))
  (expand-file-name name
                    (expand-file-name ollie--session-id (ollie--sessions-dir))))

(defun ollie--last-session-path ()
  (expand-file-name "~/.config/ollie/last-session"))

;;;; ──────────────── Low-level file helpers ────────────────

(defun ollie--fwrite (path text)
  "Write TEXT to PATH, suppressing the minibuffer message."
  (write-region text nil path nil 'silent))

(defun ollie--fread (path)
  "Return the contents of PATH as a string, or nil if it does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))


;;;; ──────────────── Session enumeration ────────────────

(defun ollie--session-ids ()
  "Return a list of all current session IDs."
  (let ((sdir (ollie--sessions-dir)))
    (when (file-directory-p sdir)
      (seq-filter
       (lambda (name) (not (string-prefix-p "." name)))
       (mapcar #'file-name-nondirectory
               (seq-filter #'file-directory-p
                           (directory-files sdir t)))))))

;;;; ──────────────── Last-session persistence ────────────────

(defun ollie--persist-session (id)
  "Upsert the {cwd}TAB{id} entry for `default-directory' in the last-session file."
  (let* ((path (ollie--last-session-path))
         (cwd  (expand-file-name default-directory))
         (raw  (ollie--fread path))
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
    (ollie--fwrite path
                   (concat (mapconcat #'identity
                                      (if found new-lines (append new-lines (list entry)))
                                      "\n")
                           "\n"))))

(defun ollie--recall-session ()
  "Return the last-used session ID for `default-directory' if it still exists, else nil."
  (let* ((path (ollie--last-session-path))
         (cwd  (expand-file-name default-directory))
         (raw  (ollie--fread path)))
    (when raw
      (catch 'found
        (dolist (line (split-string raw "\n"))
          (let ((parts (split-string line "\t" nil nil)))
            (when (and (= (length parts) 2)
                       (string= (car parts) cwd))
              (let ((id (string-trim (cadr parts))))
                (when (and (not (string-empty-p id))
                           (file-directory-p (expand-file-name id (ollie--sessions-dir))))
                  (throw 'found id))))))))))

;;;; ──────────────── Session lifecycle ────────────────

(defun ollie-new-session (&optional opts)
  "Create a new ollie session and make it current.
OPTS is an alist of option pairs; defaults to `ollie-default-session-opts'.
Returns the new session ID."
  (interactive)
  (unless (file-directory-p ollie-mount-directory)
    (user-error "ollie-9p not mounted at %s (customize `ollie-mount-directory')"
                ollie-mount-directory))
  (let* ((effective-opts (or opts ollie-default-session-opts))
         ;; cwd is required; inject it if not already present.
         (effective-opts (if (assoc "cwd" effective-opts)
                             effective-opts
                           (cons (cons "cwd" default-directory) effective-opts)))
         (before      (ollie--session-ids))
         (seen        (make-hash-table :test 'equal))
         (cmd         (mapconcat (lambda (kv)
                                   (format "%s=%s" (car kv) (cdr kv)))
                                 effective-opts " ")))
    (dolist (id before) (puthash id t seen))
    (ollie--fwrite (ollie--sessions-new-path) (concat cmd "\n"))
    (let ((deadline (+ (float-time) 5.0))
          result)
      (while (and (not result) (< (float-time) deadline))
        (dolist (id (ollie--session-ids))
          (unless (gethash id seen) (setq result id)))
        (unless result (sleep-for 0.05)))
      (unless result
        (user-error "Timed out waiting for a new ollie session"))
      (setq ollie--session-id result)
      (ollie--persist-session result)
      (message "Created ollie session %s" result)
      result)))

(defun ollie-attach (id)
  "Attach to an existing session by ID and make it current."
  (interactive
   (let ((ids (ollie--session-ids)))
     (unless ids (user-error "No ollie sessions available"))
     (list (completing-read "Attach to session: " ids nil t))))
  (unless (file-directory-p (expand-file-name id (ollie--sessions-dir)))
    (user-error "Session not found: %s" id))
  (setq ollie--session-id id)
  (ollie--persist-session id)
  (message "Attached to ollie session %s" id))

(defun ollie-kill-session ()
  "Kill the current ollie session."
  (interactive)
  (unless ollie--session-id (user-error "No active session"))
  (when (yes-or-no-p (format "Kill ollie session %s? " ollie--session-id))
    (delete-directory (expand-file-name ollie--session-id (ollie--sessions-dir)) t)
    (ollie--stop-watching)
    (setq ollie--session-id nil
          ollie--chat-size -1)
    (when-let (buf (get-buffer ollie--buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "(session killed)\n\n")
          (insert "Press 'n' or C-c C-n to start a new session.\n"))))
    (message "Session killed")))

(defun ollie-rename-session (new-name)
  "Rename the current ollie session to NEW-NAME."
  (interactive "sNew session name: ")
  (unless ollie--session-id (user-error "No active session"))
  (when (string-empty-p (string-trim new-name))
    (user-error "Empty name"))
  (let ((old-dir (expand-file-name ollie--session-id (ollie--sessions-dir)))
        (new-dir (expand-file-name new-name (ollie--sessions-dir))))
    (rename-file old-dir new-dir)
    (setq ollie--session-id new-name)
    (ollie--persist-session new-name)
    (force-mode-line-update)
    (message "Renamed session to %s" new-name)))

;;;; ──────────────── State ────────────────

(defun ollie--state ()
  "Return the current session state string, or \"unknown\" on any error."
  (condition-case nil
      (or (when-let (s (ollie--fread (ollie--session-file "state")))
            (string-trim s))
          "unknown")
    (error "unknown")))

(defun ollie--idle-p ()
  "Return non-nil if the session is currently idle."
  (ignore-errors (string= (ollie--state) "idle")))

;;;; ──────────────── Agent operations ────────────────

(defun ollie-submit (text)
  "Submit TEXT as a prompt to the current session.
The server dispatches it asynchronously."
  (when (string-empty-p (string-trim text))
    (user-error "Empty prompt"))
  (ollie--fwrite (ollie--session-file "prompt") text))

(defun ollie-queue (text)
  "Enqueue TEXT as a prompt to run after the current turn finishes."
  (when (string-empty-p (string-trim text))
    (user-error "Empty prompt"))
  (ollie--fwrite (ollie--session-file "enqueue") text))

(defun ollie-stop ()
  "Send a stop signal to the current turn."
  (interactive)
  (ollie--fwrite (ollie--session-file "ctl") "stop\n")
  (message "ollie: stopped"))

;;;; ──────────────── Diff faces ────────────────

(defface ollie-diff-added
  '((((background dark))  :foreground "#5faf5f")
    (((background light)) :foreground "#3a7a3a"))
  "Face for added lines in tool-result diff output."
  :group 'ollie)

(defface ollie-diff-removed
  '((((background dark))  :foreground "#d75f5f")
    (((background light)) :foreground "#a02020"))
  "Face for removed lines in tool-result diff output."
  :group 'ollie)

(defface ollie-diff-hunk-header
  '((((background dark))  :foreground "#5fafaf")
    (((background light)) :foreground "#1a6a6a"))
  "Face for hunk headers in tool-result diff output."
  :group 'ollie)

;;;; ──────────────── Chat display ────────────────

;; We cache the mtime so we only re-read the file when it actually changes.
;; The mode-line state is updated on every refresh tick.

(defvar-local ollie--mode-line-state "unknown"
  "Cached session state string for the mode line.")

(defvar-local ollie--mode-line-usage ""
  "Cached usage string for the mode line.")

(defun ollie--block-start-p (line)
  "Return non-nil if LINE begins a named chat block."
  (or (string-prefix-p "-> "          line)
      (string-prefix-p "user: "       line)
      (string-prefix-p "assistant: "  line)
      (string-prefix-p "retrying "    line)
      (string-prefix-p "error: "      line)
      (string-prefix-p "agent stalled" line)))

(defun ollie--colorize-diffs ()
  "Add diff highlighting to tool-result blocks in the current buffer.
Must be called after `ansi-color-apply-on-region'."
  (save-excursion
    (goto-char (point-min))
    (let ((in-result nil))
      (while (not (eobp))
        (let* ((bol (point))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol)))
          (cond
           ;; Tool call: enter result mode; don't colorize this line.
           ((string-prefix-p "-> " line)
            (setq in-result t))
           ;; Block-start: exit result mode; don't colorize this line.
           ((and in-result (ollie--block-start-p line))
            (setq in-result nil))
           ;; Inside a result block: colorize diff lines.
           (in-result
            (let ((face (cond
                         ((or (string-prefix-p "＋++" line)
                              (string-prefix-p "−--" line)) 'shadow)
                         ((string-prefix-p "＋"  line)      'ollie-diff-added)
                         ((string-prefix-p "−"   line)      'ollie-diff-removed)
                         ((string-prefix-p "@@"  line)      'ollie-diff-hunk-header))))
              (when face
                (put-text-property bol eol 'face face)))))
          (forward-line 1))))))

(defun ollie--refresh ()
  "Refresh the chat buffer if the chat file has grown on disk.
Also updates the cached mode-line state."
  (let* ((buf   (get-buffer ollie--buf))
         (chat  (condition-case nil (ollie--session-file "chat") (error nil)))
         (attrs (and chat (file-attributes chat)))
         (size  (and attrs (file-attribute-size attrs))))
    (when buf
      (with-current-buffer buf
        (setq ollie--mode-line-state (ollie--state))
        (setq ollie--mode-line-usage
              (or (condition-case nil
                      (when-let (s (ollie--fread (ollie--session-file "ctxsz")))
                        (string-trim s))
                    (error nil))
                  ""))
        ;; The chat log is append-only, so size increasing means new content.
        (when (and size (> size ollie--chat-size))
          (setq ollie--chat-size size)
          (let ((inhibit-read-only t)
                (at-end (= (point) (point-max))))
            (erase-buffer)
            (insert-file-contents chat)
            (ansi-color-apply-on-region (point-min) (point-max))
            (ollie--colorize-diffs)
            (when at-end (goto-char (point-max)))))
        (force-mode-line-update)))))

;;;; ──────────────── Polling ────────────────

;; inotify does not fire on 9P mounts, so we always use a timer.
;; The mtime check in `ollie--refresh' keeps this cheap.

(defun ollie--start-watching ()
  "Start a periodic timer to tail the chat file."
  (ollie--stop-watching)
  (setq ollie--poll-timer
        (run-with-timer ollie-poll-interval ollie-poll-interval #'ollie--refresh)))

(defun ollie--stop-watching ()
  "Cancel the refresh timer."
  (when ollie--poll-timer
    (cancel-timer ollie--poll-timer)
    (setq ollie--poll-timer nil)))

;;;; ──────────────── Input buffer ────────────────

(defvar-local ollie--input-queued nil
  "Non-nil when this input buffer will enqueue rather than submit.")

(defvar ollie-input-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'ollie-input-submit)
    (define-key m (kbd "C-c C-k") #'ollie-input-cancel)
    m)
  "Keymap for `ollie-input-mode'.")

(define-derived-mode ollie-input-mode text-mode "Ollie-Input"
  "Major mode for composing ollie prompts.
\\{ollie-input-mode-map}"
  (setq-local header-line-format
              '(:eval
                (format "  %s  ·  C-c C-c submit  ·  C-c C-k cancel"
                        (if ollie--input-queued
                            (propertize "Queue prompt" 'face 'warning)
                          (propertize "Send prompt"  'face 'success))))))

(defun ollie--open-input (&optional queued)
  "Open the prompt input window below the current window.
QUEUED non-nil means the prompt will be enqueued, not submitted immediately."
  (let ((buf (get-buffer-create ollie--ibuf)))
    (with-current-buffer buf
      (erase-buffer)
      (ollie-input-mode)
      (setq ollie--input-queued queued))
    (display-buffer buf
                    `(display-buffer-below-selected
                      (window-height . ,ollie-input-window-height)))
    (when-let (win (get-buffer-window buf t))
      (select-window win))))

(defun ollie-input-submit ()
  "Submit the contents of the input buffer as a prompt."
  (interactive)
  (let ((text   (buffer-string))
        (queued ollie--input-queued))
    (ollie--close-input-window)
    (if queued
        (ollie-queue text)
      (ollie-submit text))))

(defun ollie-input-cancel ()
  "Discard the input and close the input window."
  (interactive)
  (ollie--close-input-window))

(defun ollie--close-input-window ()
  "Kill the input buffer and delete its window."
  (when-let (win (get-buffer-window ollie--ibuf t))
    (delete-window win))
  (when-let (buf (get-buffer ollie--ibuf))
    (kill-buffer buf)))

;;;; ──────────────── Chat major mode ────────────────

(defvar ollie-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")     #'ollie-send-input)
    (define-key m (kbd "C-c C-q") #'ollie-queue-input)
    (define-key m (kbd "C-c C-c") #'ollie-stop)
    (define-key m (kbd "C-c C-k") #'ollie-kill-session)
    (define-key m (kbd "C-c C-r") #'ollie-rename-session)
    (define-key m (kbd "C-c C-n") #'ollie-new-and-open)
    (define-key m (kbd "C-c C-a") #'ollie-attach-and-open)
    (define-key m (kbd "C-c C-d") #'ollie-dired)
    (define-key m (kbd "C-c C-p") #'ollie-show-system-prompt)
    (define-key m (kbd "n")       #'ollie-new-and-open)
    (define-key m (kbd "g")       #'ollie-refresh)
    m)
  "Keymap for `ollie-mode'.")

(defun ollie-send-input ()
  "Open the input window to compose and submit a prompt."
  (interactive)
  (ollie--open-input nil))

(defun ollie-queue-input ()
  "Open the input window to compose a queued prompt."
  (interactive)
  (ollie--open-input t))

(defun ollie-refresh ()
  "Force a refresh of the chat log from disk."
  (interactive)
  (setq ollie--chat-size -1)
  (ollie--refresh))

(defun ollie-dired ()
  "Open the ollie-9p mount point in Dired."
  (interactive)
  (dired ollie-mount-directory))

(define-derived-mode ollie-mode special-mode "Ollie"
  "Major mode for the ollie chat interface.

Key bindings:
\\{ollie-mode-map}"
  :group 'ollie
  (buffer-disable-undo)
  (setq-local mode-line-format
              `("%e"
                mode-line-front-space
                (:eval
                 (propertize (format " ollie:%s " (or ollie--session-id "-"))
                             'face 'mode-line-buffer-id))
                (:eval
                 (if (string= ollie--mode-line-state "idle")
                     (propertize "idle" 'face 'success)
                   (propertize ollie--mode-line-state 'face 'warning)))
                (:eval
                 (when (not (string-empty-p ollie--mode-line-usage))
                   (concat "  " (replace-regexp-in-string "%" "%%" ollie--mode-line-usage))))
                "  "
                mode-line-end-spaces)))

;;;; ──────────────── System prompt ────────────────

(defun ollie-show-system-prompt ()
  "Display the rendered system prompt for the current session in a buffer."
  (interactive)
  (let* ((path (ollie--session-file "systemprompt"))
         (text (ollie--fread path)))
    (unless text
      (user-error "Could not read system prompt"))
    (let ((buf (get-buffer-create "*ollie-system-prompt*")))
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
(defun ollie ()
  "Open the ollie chat interface.
Attaches to the last session if it still exists; otherwise creates a new one."
  (interactive)
  (unless (file-directory-p ollie-mount-directory)
    (user-error "ollie-9p not mounted at %s  (customize `ollie-mount-directory')"
                ollie-mount-directory))
  (unless ollie--session-id
    (if-let (last (ollie--recall-session))
        (progn
          (setq ollie--session-id last)
          (message "Attached to last ollie session %s" last))
      (ollie-new-session)))
  (ollie--open-buf))

;;;###autoload
(defun ollie-new-and-open ()
  "Create a new ollie session and open the chat buffer."
  (interactive)
  (ollie-new-session)
  (ollie--open-buf))

;;;###autoload
(defun ollie-attach-and-open (id)
  "Attach to session ID and open the chat buffer."
  (interactive
   (let ((ids (ollie--session-ids)))
     (unless ids (user-error "No ollie sessions available"))
     (list (completing-read "Session: " ids nil t))))
  (ollie-attach id)
  (ollie--open-buf))

(defun ollie--open-buf ()
  "Open or switch to the *ollie* chat buffer."
  (let ((buf (get-buffer-create ollie--buf)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ollie-mode)
        (ollie-mode))
      (setq ollie--chat-size -1)
      (ollie--refresh)
      (ollie--start-watching))
    (pop-to-buffer buf)))

(provide 'ollie)
;;; ollie.el ends here
