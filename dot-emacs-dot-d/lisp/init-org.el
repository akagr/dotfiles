(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '("~/Dropbox/org/")
      org-archive-location (concat org-directory "/archive/%s::"))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  (org-appear-submarkers t)
  (org-appear-delay 0.5)
  :hook (org-mode . org-appear-mode))

;; Start once on initial org load.
;; The hook above takes care of starting it on subsequent loads.
(org-appear-mode)

(aa/leader-key-def
  "r"  '(:ignore t :which-key "org mode")
  "ra" '(org-agenda :which-key "agenda")
  "rc" '(org-capture :which-key "capture")
  "ri" '((lambda () (interactive) (org-indent-block)) :which-key "indent block")
  "rl" '(org-store-link :which-key "store Link")
  "ro" '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "open notes")
  "rp" '(org-present :which-key "present")
  "rt" '(org-babel-tangle :which-key "tangle"))

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

(use-package async
  :demand t)

(defvar *config-file* (expand-file-name "config.org" user-emacs-directory)
  "The configuration file.")

(defvar *show-async-tangle-results* nil
  "Keeps *emacs* async buffers around for later inspection.")

(defun aa/async-babel-tangle (org-file)
  "Tangles the org file asynchronously."
  (let ((init-tangle-start-time (current-time))
        (file (buffer-file-name))
        (async-quiet-switch "-q"))
    (async-start
     `(lambda ()
        (require 'ob-tangle)
        (org-babel-tangle-file ,org-file))
     (unless *show-async-tangle-results*
       `(lambda (result)
          (if result
              (message "SUCCESS: %s successfully tangled (%.2fs)."
                       ,org-file
                       (float-time
                        (time-subtract (current-time)
                                       ',init-tangle-start-time)))
            (message "ERROR: %s as tangle failed." ,org-file)))))))

(defun aa/config-tangle ()
  "Tangles the org file asynchronously."
  (aa/async-babel-tangle *config-file*))

(add-hook 'org-mode-hook
          (lambda ()
            (when (and buffer-file-truename
                       (equal (file-name-nondirectory buffer-file-truename)
                              "config.org"))
              (add-hook 'after-save-hook
                        'aa/config-tangle
                        nil 'make-it-local))))

(provide 'init-org)
