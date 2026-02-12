(use-package cond-let
  :ensure (:type git :host github :repo "tarsius/cond-let" :branch "main" :depth 1
                 :files ("*.el")))

(use-package magit
  :commands magit-status
  :config
  ;; Color remote branches differently in all magit branch completion (b b, etc.).
  ;; Advises the core completing-read wrapper to inject an affixation-function
  ;; into the metadata, which vertico uses to display candidates with faces.
  (define-advice magit-builtin-completing-read
      (:around (orig-fn prompt choices &optional predicate require-match
                        initial-input hist def) colorize-branches)
    (unless (or (bound-and-true-p helm-mode) (bound-and-true-p ivy-mode))
      (setq choices (magit--completion-table choices)))
    (let ((remote-names (ignore-errors (magit-list-remotes)))
          (ivy-sort-functions-alist nil))
      (completing-read
       prompt
       (lambda (str pred action)
         (if (eq action 'metadata)
             `(metadata
               (display-sort-function . identity)
               (cycle-sort-function . identity)
               (affixation-function
                . ,(lambda (cands)
                     (mapcar (lambda (cand)
                               (if (and remote-names
                                        (cl-some (lambda (r)
                                                   (string-prefix-p (concat r "/") cand))
                                                 remote-names))
                                   (list (propertize cand 'face 'magit-branch-remote) "" "")
                                 (list cand "" "")))
                             cands))))
           (complete-with-action action choices str pred)))
       predicate require-match initial-input hist def))))

(use-package forge
  :after magit)

(use-package transient)

(provide 'feat-git)
