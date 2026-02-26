(defun uv-python-shell-calculate-command ()
  "Calculate the string used to execute the inferior Python process."
  (format "%s %s"
          ;; `python-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          "uv run python"
          python-shell-interpreter-args))

(advice-add 'python-shell-calculate-command :override #'uv-python-shell-calculate-command)

(setq python-shell-dedicated 'project)
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-enable nil)
