;; Hide all minor modes in modeline
(use-package minions
  :config
  (minions-mode 1)
  ;; Don't show the minions menu indicator either
  (setq minions-mode-line-lighter ""))

;; Display line count when region is active
(defun my/modeline-region-line-count ()
  "Return a string showing the number of lines in the active region."
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (beg-line (line-number-at-pos beg))
           (end-line (line-number-at-pos end))
           (lines (1+ (- end-line beg-line))))
      (format " [%d line%s]" lines (if (= lines 1) "" "s")))))

;; Only show encoding if not UTF-8
(defun my/modeline-encoding ()
  "Show encoding only if it's not UTF-8."
  (let ((sys (coding-system-plist buffer-file-coding-system)))
    (unless (or (eq (plist-get sys :category) 'utf-8)
                (eq (plist-get sys :name) 'utf-8)
                (string-match-p "utf-8" (symbol-name buffer-file-coding-system)))
      (let ((eol (plist-get sys :eol-type)))
        (format " %s%s "
                (upcase (symbol-name (plist-get sys :name)))
                (if (eq eol 1) "(DOS)" ""))))))

;; Simplified position display: L:C
(setq-default mode-line-position
              '(" L%l:C%c "))

;; Build cleaner mode-line format
(setq-default mode-line-format
              '("%e"
                ;; Modified/read-only indicators
                mode-line-front-space
                mode-line-modified
                " "
                ;; Buffer name
                mode-line-buffer-identification
                ;; Region line count
                (:eval (my/modeline-region-line-count))
                " "
                ;; Position (line:column)
                mode-line-position
                " "
                ;; Major mode
                mode-line-modes
                ;; Version control
                (vc-mode vc-mode)
                " "
                ;; Encoding (only if not UTF-8)
                (:eval (my/modeline-encoding))
                ;; Misc info
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'feat-modeline)
