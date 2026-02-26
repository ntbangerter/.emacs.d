(use-package go-mode
  :ensure t
  :hook
  (before-save . gofmt-before-save)
  (go-mode . (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)))
)
