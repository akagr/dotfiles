;; https://github.com/minad/tempel
;;
;; “string” Inserts a string literal.
;; p Inserts an unnamed placeholder field.
;; n Inserts a newline.
;; > Indents with indent-according-to-mode.
;; r Inserts the current region. If no region is active, quits the containing template when jumped to.
;; r> Acts like r, but indent region.
;; n> Inserts a newline and indents.
;; & Insert newline unless there is only whitespace between line start and point.
;; % Insert newline unless there is only whitespace between point and line end.
;; o Like % but leaves the point before newline.
;; (s NAME) Inserts a named field.
;; (p PROMPT <NAME> <NOINSERT>) Insert an optionally named field with a prompt. The PROMPT is displayed directly in the buffer as default value. If NOINSERT is non-nil, no field is inserted. Then the minibuffer is used for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>) Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;;
;; Furthermore Tempel supports syntax extensions:

;; (p FORM <NAME> <NOINSERT>) Like p described above, but FORM is evaluated.
;; (FORM ...) Other Lisp forms are evaluated. Named fields are lexically bound.
;; q Quits the containing template when jumped to.

fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

lisp-mode emacs-lisp-mode lisp-interaction-mode

(lambda "(lambda (" p ")" n> r> ")")

go-mode

(func "func " p "(" p ") {" n> r> p n> r> "}")
