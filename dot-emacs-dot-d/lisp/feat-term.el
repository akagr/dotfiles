(use-package vterm
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-insert-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-visual-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-emacs-state-map (kbd "M-V") #'aa/vterm-dwim)
  :config
  (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (defun aa/vterm-dwim ()
    "Switch to/from vterm buffer based on major mode"
    (interactive)
    (if (equal major-mode 'vterm-mode)
        (switch-to-buffer (other-buffer (current-buffer) t))
      (vterm))))

(provide 'feat-term)
