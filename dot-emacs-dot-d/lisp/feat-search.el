(use-package rg
  :commands rg rg-project)

(start/leader-keys
  "s" '(rg-project :wk "search"))

(evil-collection-define-key 'normal 'rg-mode-map
  "?" 'rg-menu)

(provide 'feat-search)
