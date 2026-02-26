;; add spacing between windows
(add-to-list 'default-frame-alist '(internal-border-width . 16))
;; (set-fringe-mode 5)
(setq-default right-fringe-width 0)
(setq-default left-fringe-width 10)
(setq window-divider-default-right-width 16)
(setq window-divider-default-bottom-width 16)
(setq window-divider-default-places t)
(window-divider-mode)
(set-face-attribute 'default nil :height 140)

;; disable this on MacOS, throws an error otherwise
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil)       ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'header-line t :inherit 'default)

(column-number-mode t) ;; Show current column number in mode line

;; custom mode line
(defvar my-mode-line-prefix
  '("%e"
    (:propertize " " display (raise +0.2))
    (:propertize " " display (raise -0.2))
    (:propertize "λ  " face font-lock-comment-face)))

(setq-default mode-line-format
  `(,@my-mode-line-prefix
	mode-line-modified
	mode-line-frame-identification
	mode-line-buffer-identification

	;; Version control info
	(:eval (when-let (vc vc-mode)
			 ;; Use a pretty branch symbol in front of the branch name
			 (list (propertize "   " 'face 'font-lock-comment-face)
                   ;; Truncate branch name to 50 characters
				   (propertize (truncate-string-to-width
                                (substring vc 5) 50)
							   'face 'font-lock-comment-face))))

	;; Add space to align to the right
	(:eval (propertize
			 " " 'display
			 `((space :align-to
					  (-  (+ right right-fringe right-margin)
						 ,(+ 3
                             (string-width "%4l:3%c")))))))

	;; Line and column numbers
	(:propertize "%4l:%c" face mode-line-buffer-id)))


(use-package ace-window)


(use-package switch-window
  :bind
  (("C-x o" . switch-window)
   ("C-x C-b" . buffer-menu)))


(use-package nerd-icons)


