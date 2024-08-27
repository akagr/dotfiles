(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (expand-file-name "templates.eld" user-emacs-directory))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(provide 'feat-templates)
