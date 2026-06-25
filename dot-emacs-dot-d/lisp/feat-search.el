(use-package rg
  :commands rg rg-project)

(use-package evil-anzu
  :config
  (global-anzu-mode +1))

(start/leader-keys
  "S" '(rg-project :wk "Search project"))

(provide 'feat-search)
