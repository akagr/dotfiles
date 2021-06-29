(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 inhibit-startup-message t
 load-prefer-newer t
 ring-bell-function 'ignore
 sentence-end-double-space nil
 confirm-kill-processes nil)

(setq-default indent-tabs-mode nil
              truncate-lines t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(progn
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1))

(global-so-long-mode) ;; handles really long lines well
(electric-pair-mode) ;; auto matching brackets, parentheses etc.
(show-paren-mode) ;; show matching opening/closing parentheses
(column-number-mode) ;; show column number in mode line
(global-display-line-numbers-mode) ;; show line numbers in all buffers

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 0.3))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer aa/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,"))

(aa/leader-key-def
  "e"  '(:ignore t :which-key "emacs")
  "ee" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :which-key "open config")
  "er" '((lambda () (interactive) (load-file user-init-file)) :which-key "reload config")
  "ek" '(kill-emacs :which-key "kill emacs")

  "h"  '(help-command :which-key "help")

  "t"  '(:ignore t :which-key "toggle")
  "tt" '(counsel-load-theme :which-key "theme")
  "tw" '(toggle-truncate-lines :which-key "wrap lines")

  "c"  '(:ignore t :which-key "code")
  "cc" '(xref-find-definitions :which-key "find definitions")
  "cr" '(xref-find-references :which-key "find references"))

(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 48))

(use-package fira-code-mode
  :demand t
  :diminish
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode
  :config
  (set-face-attribute 'default nil
                      :family "FiraCode Nerd Font"
                      :height 130
                      :weight 'normal))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (load-theme 'doom-dracula t))))

;; Diminish minor modes from mode line
(use-package diminish
  :config
  (diminish 'org-indent-mode)
  (diminish 'buffer-face-mode)
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t
        evil-undo-system 'undo-fu
        evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)
        evil-emacs-state-cursor '("indian red" box)
        evil-normal-state-cursor '("indian red" box)
        evil-visual-state-cursor '("royal blue" box)
        evil-insert-state-cursor '("yellow green" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow)))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :init
  (evil-collection-init '(deadgrep dired dired-sidebar ediff ibuffer magit vterm xref)))

(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files '("~/Dropbox/org/")
        org-archive-location (concat org-directory "/archive/%s::")))

(aa/leader-key-def
  "r"  '(:ignore t :which-key "org mode")
  "ra" '(org-agenda :which-key "agenda")
  "rc" '(org-capture :which-key "capture")
  "ri" '((lambda () (interactive) (org-indent-block)) :which-key "indent block")
  "rl" '(org-store-link :which-key "store Link")
  "ro" '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "open notes")
  "rp" '(org-present :which-key "present")
  "rt" '(org-babel-tangle :which-key "tangle"))

(with-eval-after-load 'org
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
        org-src-tab-acts-natively t
        org-odd-levels-only t
        truncate-lines nil)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'variable-pitch-mode)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images))

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis " ▶")
  :hook (org-mode . org-bullets-mode))

(use-package evil-org
  :diminish
  :commands evil-org-mode
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (progn
                (let* ((variable-tuple
                        (cond ((x-list-fonts "FiraCode Nerd Font") '(:font "FiraCode Nerd Font"))
                              (nil (warn "Cannot find Firacode. Install FiraCode Nerd Font."))))
                       (base-font-color   (face-foreground 'default nil 'default))
                       (headline         `(:inherit default :weight bold :foreground ,base-font-color)))

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

              (custom-theme-set-faces
               'user
               '(variable-pitch ((t (:family "FiraCode Nerd Font" :height 140 :weight normal))))
               '(fixed-pitch ((t ( :family "FiraCode Nerd Font" :height 130))))))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
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
               '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")))
(setq prettify-symbols-unprettify-at-point t)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

(use-package org-present
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

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d) ")
  ;; Don't want to see these buffers in switcher. Rarely,
  ;; if ever have I switched to them.
  (add-to-list 'ivy-ignore-buffers "^magit")
  (add-to-list 'ivy-ignore-buffers "^:") ;; This removes the dired-sidebar buffer from list
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\*vterm\\*")
  (add-to-list 'ivy-ignore-buffers "\\*EGLOT")
  (add-to-list 'ivy-ignore-buffers "\\*eldoc*")
  (add-to-list 'ivy-ignore-buffers "\\*Warnings*")
  (add-to-list 'ivy-ignore-buffers "\\*Compile-Log\\*"))

(use-package ivy-rich
  :after ivy
  :diminish
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

(use-package hydra)
(use-package ivy-hydra
  :after (ivy hydra))

(use-package counsel
  :after ivy
  :diminish
  :config
  (counsel-mode t)
  :bind (("M-x" . counsel-M-x)))

(use-package wgrep)

(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1)
  :config
  (setq prescient-filter-method '(literal)
        prescient-sort-full-matches-first nil))

(use-package ivy-prescient
  :after (counsel counsel-projectile prescient)
  :config
  (ivy-prescient-mode t)
  (setq ivy-prescient-sort-commands t))

(aa/leader-key-def
"b" '(:ignore t :which-key "buffer")
"bb" '(counsel-projectile-switch-to-buffer :which-key "list in project")
"bc" '(kill-this-buffer :which-key "kill current")
"bd" '(aa/close-and-kill-this-pane :which-key "close current"))

(general-define-key
 :states 'normal
 "/" 'swiper)

(use-package libgit)

(use-package magit
  :defer t)

(use-package magit-libgit
  :after (magit libgit))

(aa/leader-key-def
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "status")
  "gb" '(magit-blame :which-key "blame"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package projectile
  :after evil
  :diminish
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :config
  (setq counsel-projectile-sort-files t
        counsel-projectile-sort-projects t
        counsel-projectile-sort-buffers t
        counsel-projectile-sort-directories t))

(aa/leader-key-def
  "p"  '(projectile-command-map :which-key "projectile")
  "pf" '(counsel-projectile-find-file :which-key "find file"))

(use-package perspective
  :config
  (unless (equal persp-mode t)
    (persp-mode)))

(general-define-key
 :states 'normal
 "C-b" 'persp-counsel-switch-buffer)

(aa/leader-key-def
  "bs" '(persp-switch :which-key "switch perspective")
  "bk" '(persp-kill :which-key "kill perspective")
  "bn" '(persp-next :which-key "next perspective"))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package ruby-end
  :diminish)

(use-package typescript-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(use-package elixir-mode
  :after ruby-end
  :defer t
  :config
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1)))
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package kubel
  :defer t)
(use-package kubel-evil
  :after kubel)

(aa/leader-key-def
  "k" '(kubel :which-key "k8s"))

(use-package web-mode
  :mode "\\.p?html?\\'"
  :mode "\\.eex\\'"
  :mode "\\.leex\\'"

  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t)
  )

(use-package polymode
  :mode ("\\.ex\\'" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  )

(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

(use-package yaml-mode)

(add-hook 'markdown-mode-hook #'visual-line-mode)

(use-package yasnippet
  :after company
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :hook (org-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package vterm)

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-hide-method 'reset-window-configration))

(general-define-key
 :states 'normal
 "C-t" 'vterm-toggle)

(evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-t") 'vterm-toggle)

(use-package company
  :diminish
  :init
  (setq company-idle-delay 0)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode 1))

(use-package eglot
  :after elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'eglot-ensure)
  (add-hook 'ruby-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs '(elixir-mode "/Users/akash/Downloads/elixir-ls/language_server.sh")))

;; Helps with monorepo project where projects might not be the top level
(add-hook 'project-find-functions 'aa/find-mix-project nil nil)
(add-hook 'project-find-functions 'aa/find-rails-project nil nil)

(use-package flycheck
  :diminish
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :after evil
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))

  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-subtree-line-prefix "-")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(aa/leader-key-def
  "n" '(dired-sidebar-toggle-sidebar :which-key "sidebar"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil))

(use-package ripgrep)

(use-package deadgrep)

(aa/leader-key-def
  "s" '(deadgrep :which-key "search"))

(defun aa/copy-file-path ()
  (interactive)
  (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(defalias 'copy-file-path 'aa/copy-file-path)

(defun aa/close-and-kill-this-pane ()
  "If there are multiple windows, then close this one and kill its buffer"
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun aa/find-mix-project (dir)
  "Try to locate a Elixir project root by 'mix.exs' above DIR."
  (let ((mix_root (locate-dominating-file dir "mix.exs")))
    (message "Found Elixir project root in '%s' starting from '%s'" mix_root dir)
    (if (stringp mix_root) `(transient . ,mix_root) nil)))

(defun aa/find-rails-project (dir)
  "Try to locate a Rails project root by 'Gemfile' above DIR."
  (let ((rails_root (locate-dominating-file dir "Gemfile")))
    (message "Found Rails project root in '%s' starting from '%s'" rails_root dir)
    (if (stringp rails_root) `(transient . ,rails_root) nil)))
