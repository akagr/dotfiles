(use-package diminish)

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

;; Add region line count to mode-line (insert after buffer identification)
(setq-default mode-line-format
              (let ((format (default-value 'mode-line-format)))
                ;; Insert after mode-line-buffer-identification
                (let ((pos (cl-position 'mode-line-buffer-identification format)))
                  (if pos
                      (append (cl-subseq format 0 (1+ pos))
                              '((:eval (my/modeline-region-line-count)))
                              (cl-subseq format (1+ pos)))
                    ;; Fallback: prepend if not found
                    (cons '(:eval (my/modeline-region-line-count))
                          format)))))

(provide 'feat-modeline)
