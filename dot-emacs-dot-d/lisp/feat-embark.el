(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'feat-embark)
