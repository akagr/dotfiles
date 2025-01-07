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

(provide 'feat-org)
