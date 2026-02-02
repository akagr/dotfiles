(use-package cond-let
  :ensure (:type git :host github :repo "tarsius/cond-let" :branch "main" :depth 1
                 :files ("*.el")))

(use-package magit
  :commands magit-status)

(use-package forge
  :after magit)

(use-package transient)

(provide 'feat-git)
