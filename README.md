# ellie

An [ollie](https://github.com/lneely/ollie) front-end, via [ollie-9p](https://github.com/lneely/ollie-9p), for our emacs friends. Provides a chat buffer, prompt composition, interrupt handling, and session management. The agent core, backends, and tools are handled by the ollie-9p server.

## Background

`s/sh` proved that a full interactive client could be built on top of the ollie-9p filesystem using nothing but plain file I/O. ellie asks the same question in a different environment: can the same interface be built as an Emacs package, with no knowledge of ollie's internals and no dependencies beyond Emacs itself?

The answer is yes. The implementation uses only built-in Emacs file operations: `write-region` to submit prompts, a half-second timer to tail the chat log by comparing file sizes, and `insert-file-contents` to read new content into a buffer. The ollie-9p filesystem is the entire API surface.

## Prerequisites

[olliesrv](https://github.com/lneely/ollie-9p) must be running and mounted before use:

```sh
olliesrv start
```

By default the server mounts at `~/mnt/ollie`. Set `OLLIE` to use a different path.

## Installation

Add `ellie.el` to your load path:

```elisp
(add-to-list 'load-path "/path/to/ellie")
(require 'ellie)
```

Or with `use-package`:

```elisp
(use-package ellie
  :load-path "/path/to/ellie")
```

Optionally set a mount path and default session options:

```elisp
(setq ollie-mount-directory "/custom/mount")
(setq ollie-default-session-opts '(("backend" . "ollama") ("model" . "qwen3:8b")))
```

## Usage

| Command | Description |
|---------|-------------|
| `M-x ellie` | Open chat; attach to last session for `default-directory`, or create a new one |
| `M-x ellie-new-and-open` | Force-create a new session and open chat |
| `M-x ellie-attach-and-open` | Attach to an existing session by ID |

## UI

In the `*ellie*` buffer:

| Key | Action |
|-----|--------|
| `RET` | Open input window and compose a prompt |
| `C-c C-q` | Open input window and compose a queued prompt |
| `C-c C-c` | Interrupt the running turn |
| `C-c C-k` | Kill the current session |
| `C-c C-r` | Rename the current session |
| `C-c C-n` / `n` | New session |
| `C-c C-a` | Attach to a different session |
| `C-c C-d` | Open the ollie mount in Dired |
| `g` | Force-refresh the chat log |

In the `*ellie-input*` window:

| Key | Action |
|-----|--------|
| `C-c C-c` | Submit |
| `C-c C-k` | Cancel |

The input window accepts multi-line prompts. Queued prompts are held until the current turn finishes.

The mode line shows the active session ID and current state (`idle` or the running state name).

## Code Completion

`ellie-complete` provides ghost-text code completion via a copilot session on ollie-9p. Press `M-/` to request a completion at point; the result appears as gray italic ghost text.

| Key | Action |
|-----|--------|
| `M-/` | Request completion at point (or for selected region) |
| `TAB` / `RET` | Accept the suggestion |
| Any other key | Dismiss |
| `M-/` (while ghost visible) | Request a new completion (retries preserve region) |

With an active region, the completion replaces the selected text. Without a region, it inserts after the current line.

### Configuration

| Variable | Env var | Description |
|----------|---------|-------------|
| `ellie-complete-backend` | `OLLIE_COMPLETE_BACKEND` | Backend for completion (required) |
| `ellie-complete-model` | `OLLIE_COMPLETE_MODEL` | Model for completion (required) |
| `ellie-complete-prefix-max` | — | Max chars of context before point (default: 4000) |
| `ellie-complete-suffix-max` | — | Max chars of context after point (default: 1000) |

Example:

```elisp
(setq ellie-complete-backend "ollama")
(setq ellie-complete-model "qwen3:4b")
```

Or via environment / `~/.config/ollie/env`:

```
OLLIE_COMPLETE_BACKEND=ollama
OLLIE_COMPLETE_MODEL=qwen3:4b
```

## License

GPLv3