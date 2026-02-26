(use-package emacs
  :init
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-visual-line-mode 1)
  (global-display-line-numbers-mode)
  ;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; (set-frame-font "Iosevka Fixed 13")
  (set-frame-font "Iosevka Nerd Font")
  (setq initial-scratch-message ";; How perfect is this\n;; How lucky are we\n\n")
  (setq initial-buffer-choice t)
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (setq exec-path (append exec-path '("~/.local/bin/")))
  
  :bind
  ("M-p" . beginning-of-defun)
  ("M-n" . end-of-defun)
  ("<backtab>" . indent-rigidly-left)
  
  :hook
  (after-init . toggle-frame-fullscreen))


(use-package paren
  :ensure nil
  :init
  (electric-pair-mode t)
  (setq show-paren-delay 0)
  
  :config
  (show-paren-mode +1))


(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
    ("RET" . dired-find-alternate-file)
    ("^" . (lambda() (interactive) (find-alternate-file "..")))))


(use-package eshell
  :ensure nil
  
  :bind
  ("C-c t" . eshell))


(use-package hungry-delete
  :bind
  (("C-c DEL" . hungry-delete-backward)))
