(use-package vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

(provide 'feat-term)
