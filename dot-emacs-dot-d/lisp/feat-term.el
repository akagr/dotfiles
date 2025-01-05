(use-package vterm
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-insert-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-visual-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-emacs-state-map (kbd "M-V") #'aa/vterm-dwim)
  :config
  (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
  (with-eval-after-load 'feat-layout-restore
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
    (defun aa/vterm-dwim ()
      "Switch to/from vterm buffer based on major mode, intelligently managing window layout"
      (interactive)
      (if (equal major-mode 'vterm-mode)
          (progn
            (switch-to-buffer (other-buffer (current-buffer) t))
            (layout-restore)
            (layout-delete-current))
        (layout-save-current)
        (delete-other-windows)
        (vterm)))))

(provide 'feat-term)
