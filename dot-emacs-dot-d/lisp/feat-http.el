;; Installing straight from github because ef-themes don't
;; seem to be available on elpa.

(use-package verb
  :mode ("\\.org\\'" . org-mode)
  :config

  (defun remove-newlines (rs)
    ;; Remove newlines and make the request in graphql format
    (oset rs body (replace-regexp-in-string "\n" " " (oref rs body)))
    rs)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'feat-appearance)
