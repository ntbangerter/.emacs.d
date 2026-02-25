;;; -*- lexical-binding: t -*-
;;; ============================================================
;;; vterm interactive shell emulation tuned for AI CLIs
;;; ============================================================

(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000))

;;; Coding agent vterm buffers
(defcustom my-agent-commands
  '(("claude"        . "claude")
    ("claude-docker" . "docker sandbox run claude")
    ("aider"         . "aider"))
  "Alist mapping display names to CLI commands for coding agents."
  :type '(alist :key-type string :value-type string)
  :group 'tools)

(defvar-local my-agent--active nil
  "Non-nil in vterm buffers managed by `my-agent'.")
(defvar-local my-agent--multiline-buffer nil)
(defvar-local my-agent--multiline-timer nil)

(defun my-agent--multiline-filter (orig-fun process input)
  "Buffer rapid redraw sequences to prevent flickering in agent vterm buffers."
  (if (not (buffer-local-value 'my-agent--active (process-buffer process)))
      (funcall orig-fun process input)
    (let ((has-clear-line  (string-match-p "\033\\[K" input))
          (has-cursor-pos  (string-match-p "\033\\[\\([0-9]+\\(;[0-9]+\\)?\\)?[Hf]" input))
          (has-cursor-move (string-match-p "\033\\[[0-9]*[ABCD]" input))
          (escape-count    (cl-count ?\033 input)))
      (if (or (and (>= escape-count 3)
                   (or has-clear-line has-cursor-pos has-cursor-move))
              (with-current-buffer (process-buffer process)
                my-agent--multiline-buffer))
          (with-current-buffer (process-buffer process)
            (setq my-agent--multiline-buffer
                  (concat my-agent--multiline-buffer input))
            (when my-agent--multiline-timer
              (cancel-timer my-agent--multiline-timer))
            (setq my-agent--multiline-timer
                  (run-at-time 0.01 nil
                               (lambda (buf)
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (let ((data my-agent--multiline-buffer))
                                       (setq my-agent--multiline-buffer nil
                                             my-agent--multiline-timer nil)
                                       (funcall orig-fun process data)))))
                               (process-buffer process))))
        (funcall orig-fun process input)))))

(defun my-agent--update-cursor ()
  (setq-local cursor-type (if vterm-copy-mode 'hollow nil)))

(add-hook 'vterm-copy-mode-hook #'my-agent--update-cursor)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))

(defun my-agent--project-root ()
  "Return the VC root of the current buffer, or `default-directory'."
  (or (vc-root-dir) default-directory))

(defun my-agent (name &optional new-session)
  "Spin up a vterm buffer for a coding agent.
Prompts for agent NAME from `my-agent-commands'.
Reuses an existing buffer for the current project if one exists.
With prefix arg NEW-SESSION, always open a fresh buffer."
  (interactive
   (list (completing-read "Agent: " my-agent-commands nil t)
         current-prefix-arg))
  (let* ((command     (cdr (assoc name my-agent-commands)))
         (root        (my-agent--project-root))
         (base-name   (format "*%s: %s*" name (abbreviate-file-name root)))
         (buf         (if new-session
                          (generate-new-buffer base-name)
                        (get-buffer-create base-name)))
         (vterm-shell command))
    (switch-to-buffer buf)
    (unless (get-buffer-process buf)
      (setq-local default-directory root)
      (vterm-mode)
      (setq-local my-agent--active t)
      (advice-add 'vterm--filter :around #'my-agent--multiline-filter))))
