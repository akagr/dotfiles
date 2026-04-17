(defun aa/vterm-pin-cursor-color ()
  "Keep the vterm cursor pastel red, even when inner programs send OSC 12."
  (face-remap-add-relative 'cursor :background "#ff9999"))

(defun aa/ignore-cursor-color-in-vterm (orig &rest args)
  "Suppress cursor-color overrides coming from vterm buffers."
  (unless (derived-mode-p 'vterm-mode)
    (apply orig args)))

(use-package vterm
  :after evil
  :init
  (define-key evil-normal-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-insert-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-visual-state-map (kbd "M-V") #'aa/vterm-dwim)
  (define-key evil-emacs-state-map (kbd "M-V") #'aa/vterm-dwim)
  :config
  (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
  (add-hook 'vterm-mode-hook #'aa/vterm-pin-cursor-color)
  (advice-add 'set-cursor-color :around #'aa/ignore-cursor-color-in-vterm)
  (with-eval-after-load 'feat-layout-restore
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
    (defvar aa/vterm-dwim-origin nil)
    (defun aa/vterm-dwim ()
      "Switch to/from vterm buffer based on major mode, intelligently managing window layout"
      (interactive)
      (unless (and (equal major-mode 'vterm-mode)
                   (not (string-match-p "^\\*claude" (buffer-name))))
        (setq aa/vterm-dwim-origin (current-buffer)))
      (when (window-parameter nil 'window-side)
        (select-window (car (seq-filter
                             (lambda (w) (not (window-parameter w 'window-side)))
                             (window-list)))))
      (if (and (equal major-mode 'vterm-mode)
               (not (string-match-p "^\\*claude" (buffer-name))))
          (progn
            (switch-to-buffer (other-buffer (current-buffer) t))
            (layout-restore)
            (layout-delete-current)
            (when (buffer-live-p aa/vterm-dwim-origin)
              (let ((win (get-buffer-window aa/vterm-dwim-origin)))
                (when win (select-window win)))))
        (layout-save-current)
        (delete-other-windows)
        (vterm)))))

(provide 'feat-term)
