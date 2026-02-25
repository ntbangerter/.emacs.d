;; Drop custom theme files into ~/.emacs.d/themes/ and update my-theme-dark below.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defvar my-theme-light 'doom-flatwhite)
(defvar my-theme-dark  'zenburn)
(defvar my-theme-current 'light)

(defun my-theme-apply-light-faces ()
  (set-face-attribute 'line-number nil :slant 'italic :background 'unspecified)
  (set-face-attribute 'window-divider nil :foreground "#f7f3ee")
  ;; Active modeline: soft blue to stand out from inactive
  (set-face-attribute 'mode-line nil :background "#dde4f2")
  (set-face-attribute 'mode-line-inactive nil :background "#f1ece4")
  ;; diff-hl: use flatwhite's blend colors for visible but harmonious indicators
  (with-eval-after-load 'diff-hl
    (set-face-attribute 'diff-hl-insert nil :background "#84bd00" :foreground "#84bd00")
    (set-face-attribute 'diff-hl-change nil :background "#f08c00" :foreground "#f08c00")
    (set-face-attribute 'diff-hl-delete nil :background "#f00000" :foreground "#f00000")))

(defun my-theme-apply-dark-faces ()
  (set-face-attribute 'line-number nil :slant 'italic :background 'unspecified)
  (set-face-attribute 'fringe nil :background 'unspecified)
  (set-face-attribute 'window-divider nil :foreground "#3f3f3f")
  (set-face-attribute 'window-divider-first-pixel nil :foreground "#3f3f3f")
  (set-face-attribute 'window-divider-last-pixel nil :foreground "#3f3f3f")
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (with-eval-after-load 'diff-hl
    (set-face-attribute 'diff-hl-insert nil :background "#7f9f7f" :foreground "#7f9f7f")
    (set-face-attribute 'diff-hl-change nil :background "#dfaf8f" :foreground "#dfaf8f")
    (set-face-attribute 'diff-hl-delete nil :background "#cc9393" :foreground "#cc9393")))

(defun my-claude-sync-theme (variant)
  "Write VARIANT ('light or 'dark) into ~/.claude/settings.json."
  (let* ((file (expand-file-name "~/.claude/settings.json"))
         (settings (if (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer))
                     (make-hash-table :test 'equal))))
    (puthash "theme" (symbol-name variant) settings)
    (with-temp-file file
      (insert (json-serialize settings)))))

(defun my-theme-set-light ()
  "Switch to the light theme."
  (interactive)
  (disable-theme my-theme-dark)
  (load-theme my-theme-light t)
  (my-theme-apply-light-faces)
  (setq my-theme-current 'light)
  (my-claude-sync-theme 'light))

(defun my-theme-set-dark ()
  "Switch to the dark theme."
  (interactive)
  (disable-theme my-theme-light)
  (load-theme my-theme-dark t)
  (my-theme-apply-dark-faces)
  (setq my-theme-current 'dark)
  (my-claude-sync-theme 'dark))

(defun my-theme-toggle ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq my-theme-current 'light)
      (my-theme-set-dark)
    (my-theme-set-light)))

(define-prefix-command 'my-theme-map)
(global-set-key (kbd "C-c v") 'my-theme-map)
(define-key my-theme-map (kbd "l") #'my-theme-set-light)
(define-key my-theme-map (kbd "d") #'my-theme-set-dark)
(define-key my-theme-map (kbd "t") #'my-theme-toggle)

(use-package zenburn-theme :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme my-theme-light t)
  (my-theme-apply-light-faces))
