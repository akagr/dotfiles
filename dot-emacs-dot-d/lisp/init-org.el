(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '("~/Dropbox/org/")
      org-archive-location (concat org-directory "/archive/%s::"))

(aa/leader-key-def
  "r"  '(:ignore t :which-key "org mode")
  "ra" '(org-agenda :which-key "agenda")
  "rc" '(org-capture :which-key "capture")
  "ri" '((lambda () (interactive) (org-indent-block)) :which-key "indent block")
  "rl" '(org-store-link :which-key "store Link")
  "ro" '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "open notes")
  "rp" '(org-present :which-key "present")
  "rt" '(org-babel-tangle :which-key "tangle"))

(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-src-tab-acts-natively t
      truncate-lines nil)

;; Turn on visual line mode to wrap lines.
(visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Turn on variable pitch mode to use different scale for headings
(variable-pitch-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; If the source block contains code that outputs images,
;; show them inline in results area.
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("☯" "✸" "✿" "✜" "◆" "◉" "▶" "○"))
  :hook (org-mode . org-bullets-mode))

;; Start once on initial org load.
;; The hook above takes care of starting it on subsequent loads.
(org-bullets-mode)

(use-package evil-org
  :diminish
  :hook (org-mode . evil-org-mode)
  :init
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Start once on initial org load.
;; The hook above takes care of starting it on subsequent loads.
(evil-org-mode)

(defun aa/apply-org-headline-styles ()
  "Applies org font styles to supplied frame"
  (let* ((variable-tuple
          (cond ((x-list-fonts "JetBrains Mono") '(:font "JetBrains Mono"))
                (nil (warn "Cannot find JetBrains Mono."))))
         (headline '(:inherit default :weight bold)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.75 :underline nil)))))))

;; Apply font styles for org mode
(if window-system (aa/apply-org-headline-styles))

(defun aa/apply-org-fixed-pitch-styles ()
  "Applies variable fonts to different org elements"
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(if window-system (aa/apply-org-fixed-pitch-styles))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")))
(setq prettify-symbols-unprettify-at-point t)

(prettify-symbols-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(use-package org-present
  :commands (org-present)
  :config
  (add-hook 'org-present-mode-hook
               (lambda ()
                 (display-line-numbers-mode -1)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (display-line-numbers-mode +1)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write))))

(use-package org-drill
  :commands (org-drill))

(org-reload)
(provide 'init-org)
