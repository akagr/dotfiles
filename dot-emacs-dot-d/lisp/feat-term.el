(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode)
  :config
  (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode -1))))

(provide 'feat-term)
