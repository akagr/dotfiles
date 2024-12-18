(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (markdown-mode . smartparens-mode)
         (lisp-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (lisp-interaction-mode . smartparens-strict-mode))
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package evil-cleverparens
  :after smartparens
  :hook ((lisp-mode . evil-cleverparens-mode)
         (emacs-lisp-mode . evil-cleverparens-mode)
         (lisp-interaction-mode . evil-cleverparens-mode))
  :custom
  (evil-cleverparens-use-additional-bindings nil))

(provide 'feat-smartparens)
