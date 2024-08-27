fundamental-mode

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")
(banner comment-start (make-string (+ (length str) 2) ?=) n>
        comment-start " " (s str) n>
        comment-start (make-string (+ (length str) 2) ?=))

lisp-mode emacs-lisp-mode lisp-interaction-mode

(lambda "(lambda (" p ")" n> r> ")")
