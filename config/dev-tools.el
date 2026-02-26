;;; ============================================================
;;; Magit — git interface
;;; ============================================================

(use-package magit
  :defer t
  :custom
  ;; Show magit status in the current window rather than splitting
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Automatically refresh magit status buffer when files change
  (magit-auto-revert-mode t)
  ;; Show fine-grained word diffs in hunks
  (magit-diff-refine-hunk 'all)
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-current)
   ("C-c g f" . magit-log-buffer-file))
  :config
  (setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window))))))

;;; ============================================================
;;; diff-hl — highlight uncommitted changes in the fringe
;;; ============================================================

(use-package diff-hl
  :demand t
  :config
  ;; Enable globally
  (global-diff-hl-mode)
  ;; Update indicators live as you edit, not just on save
  (diff-hl-flydiff-mode)

  ;; Show diffs in the margin in terminal Emacs (no fringe available)
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Show indicators in Dired buffers too
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Update indicators after a Magit operation without needing a save
  (add-hook 'magit-pre-refresh-hook  'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  :bind
  ;; Navigate between hunks
  (("C-c v n" . diff-hl-next-hunk)
   ("C-c v p" . diff-hl-previous-hunk)
   ("C-c v r" . diff-hl-revert-hunk)
   ("C-c v d" . diff-hl-show-hunk)))


;;; ============================================================
;;; eglot
;;; ============================================================

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(
    (python-mode python-ts-mode)
    ;; "uv" "run" "basedpyright-langserver" "--stdio"
    "uv" "run" "ty" "server" :initializationOptions (:experimental (:rename t))
  ))
  :hook
  (go-mode . eglot-ensure)
  (python-mode . eglot-ensure))
