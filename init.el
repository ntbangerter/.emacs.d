;;; ============================================================
;;; Package bootstrap (uses built-in package.el + use-package)
;;; ============================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure use-package is available (built-in since Emacs 29)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;; ============================================================
;;; Load Files
;;; ============================================================

(load "~/.emacs.d/general.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/completion-ui.el")
(load "~/.emacs.d/ai-term.el")
(load "~/.emacs.d/dev-tools.el")
(load "~/.emacs.d/theme.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/go.el")

;;; ============================================================
;;; EOF
;;; ============================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
