(use-package git-commit)

(use-package magit
  :after git-commit
  :commands magit-status)

(use-package transient)

(provide 'feat-git)
