(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.

  :hook
  (org-mode . org-indent-mode))

(use-package toc-org
  :after org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam
  :custom
  (org-roam-directory "~/roam")
  (org-roam-complete-everywhere t)

  :config
  (org-roam-db-autosync-enable))

(start/leader-keys
  "o" '(:ignore t :wk "Org")
  "o o" '(org-roam-node-find :wk "Find node")
  "o i" '(org-roam-node-insert :wk "Insert node")
  "o s" '(org-roam-db-sync :wk "Sync DB"))

(provide 'feat-org)
