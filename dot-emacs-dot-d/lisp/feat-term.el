(defun aa/vterm-pin-cursor-color ()
  "Keep the vterm cursor pastel red, even when inner programs send OSC 12."
  (face-remap-add-relative 'cursor :background "#ff9999"))

(defun aa/ignore-cursor-color-in-vterm (orig &rest args)
  "Suppress cursor-color overrides coming from vterm buffers."
  (unless (derived-mode-p 'vterm-mode)
    (apply orig args)))

(defun aa/vterm-send-shift-return ()
  "Send ESC+CR, the byte sequence CLI TUIs (e.g. copilot, claude) map to
shift+enter for inserting a newline.  vterm otherwise sends a plain RET for
both Enter and Shift+Enter, so the program cannot tell them apart."
  (interactive)
  (process-send-string vterm--process "\e\r"))

(defvar aa/vterm-dwim-origin nil)

(defun aa/vterm-dwim ()
  "Switch to/from vterm buffer based on major mode, intelligently managing window layout"
  (interactive)
  (require 'feat-layout-restore)
  (unless (and (equal major-mode 'vterm-mode)
               (not (string-match-p "^\\*agent:" (buffer-name))))
    (setq aa/vterm-dwim-origin (current-buffer)))
  (when (window-parameter nil 'window-side)
    (select-window (car (seq-filter
                         (lambda (w) (not (window-parameter w 'window-side)))
                         (window-list)))))
  (if (and (equal major-mode 'vterm-mode)
           (not (string-match-p "^\\*agent:" (buffer-name))))
      (progn
        (switch-to-buffer (other-buffer (current-buffer) t))
        (layout-restore)
        (layout-delete-current)
        (when (buffer-live-p aa/vterm-dwim-origin)
          (let ((win (get-buffer-window aa/vterm-dwim-origin)))
            (when win (select-window win)))))
    (layout-save-current)
    (delete-other-windows)
    (vterm)))

;; Bind M-V independently of the vterm package. `aa/vterm-dwim' autoloads
;; vterm on demand, so the binding must exist as soon as evil is available,
;; regardless of whether/when the vterm package itself has loaded or built.
(with-eval-after-load 'evil
  (dolist (map (list evil-normal-state-map
                     evil-insert-state-map
                     evil-visual-state-map
                     evil-emacs-state-map))
    (define-key map (kbd "M-V") #'aa/vterm-dwim)))

(use-package vterm
  :after evil
  :demand t
  :config
  (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
  (add-hook 'vterm-mode-hook #'aa/vterm-pin-cursor-color)
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key vterm-mode-map (kbd "<S-return>") #'aa/vterm-send-shift-return)
  (advice-add 'set-cursor-color :around #'aa/ignore-cursor-color-in-vterm))

(start/leader-keys
  "v" '(vterm :wk "Vterm"))

(provide 'feat-term)
