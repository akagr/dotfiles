(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(defun aa/unwanted-buffer-p (&optional buffer)
  "Check if BUFFER (or current buffer) is an unwanted buffer type."
  (with-current-buffer (or buffer (current-buffer))
    (or (eq major-mode 'dired-mode)
        (eq major-mode 'vterm-mode)
        (derived-mode-p 'magit-mode)
        (string-match-p "^\\*claude\\|^\\*Claude\\|minibuffer" (buffer-name))
        (string-prefix-p " " (buffer-name))))) ; skip hidden buffers

(defun aa/skip-unwanted-buffers-advice (&rest _args)
  "Advice to skip unwanted buffers (dired, claude, vterm, magit, minibuffer) when switching after killing current buffer."
  (when (and (aa/unwanted-buffer-p)
             (> (length (buffer-list)) 1))
    (let ((buffers (buffer-list)))
      (catch 'found
        (dolist (buffer buffers)
          (unless (aa/unwanted-buffer-p buffer)
            (switch-to-buffer buffer)
            (throw 'found buffer)))
        ;; If no suitable buffer found, switch to scratch
        (switch-to-buffer "*scratch*")))))

(dolist (func '(kill-current-buffer
                kill-this-buffer
                evil-delete-buffer
                project-kill-buffers))
  (advice-add func :after #'aa/skip-unwanted-buffers-advice))

(provide 'feat-buffer)
