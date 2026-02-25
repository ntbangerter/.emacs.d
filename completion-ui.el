;;; ============================================================
;;; Orderless — flexible space-separated completion matching
;;; ============================================================

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; ============================================================
;;; Vertico — vertical minibuffer completion UI
;;; ============================================================

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)          ; wrap around at top/bottom
  (vertico-count 15))        ; number of candidates to show

;; Persist minibuffer history across sessions
(use-package savehist
  :init
  (savehist-mode))

;;; ============================================================
;;; Marginalia — annotations in the minibuffer
;;; ============================================================

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  :bind
  ;; Allow cycling annotation detail level in the minibuffer
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; ============================================================
;;; Corfu — in-buffer completion popup
;;; ============================================================

(use-package corfu
  :custom
  (corfu-auto t)               ; show popup automatically
  (corfu-auto-delay 0.2)       ; seconds before popup appears
  (corfu-auto-prefix 2)        ; min prefix length to trigger
  (corfu-cycle t)              ; wrap around candidates
  (corfu-quit-at-boundary nil) ; don't quit at word boundary
  (corfu-separator ?\s)        ; separator for orderless with corfu
  (corfu-quit-no-match t)      ; quit if no match
  (corfu-preview-current nil)  ; don't insert preview inline
  :init
  (global-corfu-mode)
  ;; Persist completion history
  (corfu-history-mode))

;; Cape — extra completion-at-point sources for Corfu
(use-package cape
  :init
  ;; Add useful capf backends globally
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;; ============================================================
;;; Treemacs — project sidebar
;;; ============================================================

(use-package treemacs
  :defer t
  :custom
  (treemacs-width 35)
  (treemacs-indentation 2)
  (treemacs-show-hidden-files t)
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window t) ; prevent treemacs being used as target for splits
  (treemacs-user-mode-line-format `(,@my-mode-line-prefix "Treemacs"))
  :config
  ;; Follow the currently open file in treemacs
  (treemacs-follow-mode t)
  ;; Show git status decorations
  (treemacs-git-mode 'deferred)
  ;; Keep treemacs in sync with file changes on disk
  (treemacs-filewatch-mode t)
  ;; Collapse directories with only one child
  (treemacs-fringe-indicator-mode 'always)
  :hook
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t t"   . treemacs)
   ("C-x t d"   . treemacs-select-directory)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-f" . treemacs-find-file)
   ("C-x t M-f" . treemacs-find-tag)))

;; Projectile integration (optional but recommended)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Magit integration — update git decorations after magit operations
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Nerd-icons integration for treemacs
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))
