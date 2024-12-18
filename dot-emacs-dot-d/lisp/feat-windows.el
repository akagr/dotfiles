;; Lifted from https://emacs.dyerdwelling.family/emacs/20241213115239-emacs--emacs-core-window-jumping-between-two-windows/
(defun aa/quick-window-jump ()
  "Jump to a window by typing its assigned character label.
If there are only two windows, jump directly to the other window."
  (interactive)
  (let* ((window-list (window-list nil 'no-mini)))
    (if (<= (length window-list) 2)
        ;; If there are only two windows, switch to the other one directly.
        ;; If there are less than two windows, this won't do anything.
        (select-window (other-window-for-scrolling))
      ;; Otherwise, show the key selection interface.
      (let* ((my/quick-window-overlays nil)
             (sorted-windows (sort window-list
                                   (lambda (w1 w2)
                                     (let ((edges1 (window-edges w1))
                                           (edges2 (window-edges w2)))
                                       (or (< (car edges1) (car edges2))
                                           (and (= (car edges1) (car edges2))
                                                (< (cadr edges1) (cadr edges2))))))))
             (window-keys (seq-take '("j" "k" "l" ";" "a" "s" "d" "f")
                                    (length sorted-windows)))
             (window-map (cl-pairlis window-keys sorted-windows)))
        (setq my/quick-window-overlays
              (mapcar (lambda (entry)
                        (let* ((key (car entry))
                               (window (cdr entry))
                               (start (window-start window))
                               (overlay (make-overlay start start (window-buffer window))))
                          (overlay-put overlay 'after-string
                                       (propertize (format "[%s]" key)
                                                   'face '(:foreground "white" :background "blue" :weight bold)))
                          (overlay-put overlay 'window window)
                          overlay))
                      window-map))
        (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
          (mapc #'delete-overlay my/quick-window-overlays)
          (setq my/quick-window-overlays nil)
          (when-let ((selected-window (cdr (assoc (char-to-string key) window-map))))
            (select-window selected-window)))))))

(use-package emacs
  :ensure nil
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "M-o") #'aa/quick-window-jump)
  (define-key evil-insert-state-map (kbd "M-o") #'aa/quick-window-jump)
  (define-key evil-visual-state-map (kbd "M-o") #'aa/quick-window-jump)
  (define-key evil-emacs-state-map (kbd "M-o") #'aa/quick-window-jump))

(provide 'feat-windows)
