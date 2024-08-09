(use-package treesit
  :ensure nil)

(use-package treesit-auto
  :after treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(provide 'feat-treesitter)
