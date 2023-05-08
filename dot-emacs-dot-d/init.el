(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq auto-save-default nil
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
(column-number-mode) ;; show column number in mode line
(global-auto-revert-mode) ;; reflect changes on disk to file automatically
(show-paren-mode) ;; show matching delimiters
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

(defun aa/copy-file-path ()
  "Copy file path of current buffer relative to project root."
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

(defun aa/print-startup-time ()
  "Log emacs startup time"
  (interactive)
  (message "Emacs started in %s with %d garbage collections."
           (format
            "%.2f seconds"
            (float-time
             (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun aa/dashcase (str)
  "Converts a string to dash case.

   Example:
   (aa/dashcase \"Hello World\")
   => \"hello-world\" "
  (let ((down (downcase str)))
    (replace-regexp-in-string "\\([^A-Za-z]\\)" "-" down)))

(cl-loop for file in '("/opt/homebrew/bin/fish"
                       "/usr/local/bin/fish"
                       "/bin/fish"
                       "/opt/homebrew/bin/zsh"
                       "/usr/local/bin/zsh"
                       "/bin/zsh"
                       "/opt/homebrew/bin/bash"
                       "/usr/local/bin/bash"
                       "/bin/bash")
         when (file-exists-p file)
         do (progn
              (setq shell-file-name file)
              (cl-return)))
(setenv "SHELL" shell-file-name)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
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
  "ee" '((lambda () (interactive) (find-file (expand-file-name "config.org" user-emacs-directory))) :which-key "open config")
  "er" '((lambda () (interactive) (load-file user-init-file)) :which-key "reload config")
  "ek" '(kill-emacs :which-key "kill emacs")

  "f"  '(:ignore t :which-key "file")
  "ff" '(find-file :which-key "find")

  "t"  '(:ignore t :which-key "toggle")
  "tt" '(load-theme :which-key "theme")
  "tw" '(toggle-truncate-lines :which-key "wrap lines")

  "c"  '(:ignore t :which-key "code")
  "ca" '(align-regexp :which-key "align regex")
  "cc" '(xref-find-definitions :which-key "find definitions")
  "ce" '(eval-last-sexp :which-key "eval last sexp")
  "cr" '(xref-find-references :which-key "find references"))

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point
             helpful-command
             helpful-macro
             helpful-function))
(aa/leader-key-def
  "h"  '(help-command :which-key "help")
  "hf" '(helpful-callable :which-key "functions")
  "hg" '(google :which-key "google")
  "hk" '(helpful-key :which-key "key-bindings")
  "hm" '(consult-minor-mode-menu :which-key "consult-minor-mode-menu")
  "hv" '(helpful-variable :which-key "variables"))

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
        evil-insert-state-cursor '("yellow green" box)
        evil-replace-state-cursor '("red" box)
        evil-operator-state-cursor '("red" hollow)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package evil-collection
  :init
  (evil-collection-init
   '(dired
     dired-sidebar
     ediff
     embark
     grep
     helpful
     ibuffer
     magit
     man
     occur
     process-menu
     rg
     sly
     smerge
     vterm
     wgrep
     xref
     ztree)))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'init-org)))

(with-eval-after-load 'org
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        truncate-lines nil))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (org-align-tags t))
                      nil t)))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Evil bindings to work with folds
;; za - toggle fold
;; zc - close fold
;; zo - open fold
;; zm - close all folds
;; zr - open all folds

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '(file (styles partial-completion))))

(use-package consult
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :custom
  ;; This effectively disables the comfirmation for anything not
  ;; listed.
  (embark-allow-edit-commands
   '(shell-command async-shell-command pp-eval-expression))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        embark-quit-after-action '((kill-buffer . nil)
                                   (t . nil)))

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (define-key embark-file-map     (kbd "o") #'find-file-other-window)
  (define-key embark-buffer-map   (kbd "o") #'switch-to-buffer-other-window))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ;; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(general-define-key
 :states 'normal
 "C-s" 'consult-line
 "C-h m" 'consult-minor-mode-menu)

(use-package magit
  :commands (magit magit-status magit-blame))

(aa/leader-key-def
  "g" '(:ignore t :which-key "git")
  "gb" '(magit-blame :which-key "blame")
  "gc" '(magit-clone :which-key "clone")
  "gf" '(magit-log-buffer-file :which-key "file history")
  "gs" '(magit-status :which-key "status"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package with-editor
  :after magit
  :config
  (define-key (current-global-map)
              [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
              [remap shell-command] 'with-editor-shell-command)
  (add-hook 'vterm-mode-hook 'with-editor-export-editor))

(use-package ztree
  :config
  (defun diff-directories ()
    "Thin wrapper over ztree-diff to be more discoverable"
    (interactive)
    (call-interactively 'ztree-diff)))

(use-package ace-window
  :commands (ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background nil))

(aa/leader-key-def
"b"   '(:ignore t :which-key "buffer")
"bb"  '(consult-buffer :which-key "list buffers")
"bB"  '(ibuffer :which-key "ibuffer")
"bc"  '(kill-this-buffer :which-key "kill current")
"bd"  '(aa/close-and-kill-this-pane :which-key "close current"))

(general-define-key
 "M-o" 'ace-window)

(aa/leader-key-def
  "w"   '(:ignore t :which-key "window")
  "w="  '(balance-windows :which-key "balance")
  "w>"  '(evil-window-increase-width :which-key "width+︎")
  "w<"  '(evil-window-decrease-width :which-key "width-")
  "w+"  '(evil-window-increase-height :which-key "height+")
  "w-"  '(evil-window-decrease-height :which-key "height-")
  "wc"  '(evil-window-delete :which-key "close")
  "wq"  '(evil-window-delete :which-key "close")
  "wh"  '(evil-window-split :which-key "horizontal")
  "wo"  '(delete-other-windows :which-key "only")
  "wr"  '(evil-window-rotate-upwards :which-key "rotate")
  "ww"  '(evil-window-vsplit :which-key "vertical")
  "wm"  '(:ignore t :which-key "move")
  "wmh" '(evil-window-move-far-left :which-key "left")
  "wmj" '(evil-window-move-very-bottom :which-key "down")
  "wmk" '(evil-window-move-very-top :which-key "up")
  "wml" '(evil-window-move-far-right :which-key "right"))

(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(setq ibuffer-show-empty-filter-groups nil)

(defun aa/dired-sort-directories ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (aa/dired-sort-directories))

(use-package projectile
  :after evil
  :init
  (setq projectile-enable-caching t)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1))

(aa/leader-key-def
  "p"  '(projectile-command-map :which-key "projectile"))

(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode)
  :custom
  (rubocop-autocorrect-on-save t))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package prettier
  :hook (typescript-mode javascript-mode))

(use-package elixir-mode
  :mode "\\.exs?\\'"
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package web-mode
  :mode "\\.p?html?\\'"
  :mode "\\.eex\\'"
  :mode "\\.leex\\'"

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-indentation t))

(use-package yaml-mode)

(use-package markdown-mode
  :mode "\\.md\\'")

(add-hook 'markdown-mode-hook #'visual-line-mode)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package jenkinsfile-mode
  :mode ("\\`Jenkinsfile'" . typescript-mode))

(use-package smartparens
  :init
  (smartparens-global-mode)
  :hook ((emacs-lisp-mode lisp-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (custom-set-variables
   '(sp-override-key-bindings
     '(("M-T" . sp-transpose-sexp)
       ("M-(" . sp-wrap-round)
       ("M-{" . sp-wrap-curly)
       ("M-[" . sp-wrap-square)
       ("M-r" . sp-raise-sexp)
       ("M-<right>" . sp-forward-slurp-sexp)
       ("M-<left>" . sp-backward-slurp-sexp)
       ("M-S-<right>" . sp-forward-barf-sexp)
       ("M-S-<left>" . sp-backward-barf-sexp)
       ("C-<up>" . sp-backward-up-sexp)
       ("C-S-<up>" . sp-up-sexp)
       ("C-<down>" . sp-down-sexp)
       ("C-S-<down>" . sp-backward-down-sexp)
       ("C-<right>" . sp-forward-sexp)
       ("C-<left>" . sp-backward-sexp)))))

(use-package evil-smartparens
  :after (smartparens)
  :hook ((smartparens-strict-mode) . evil-smartparens-mode))

(use-package sly
  :commands sly
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly-asdf
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package banner-comment
  :commands banner-comment)

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package vterm
  :commands (vterm)
  :config
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'vterm-mode-hook (lambda () (setq mode-line-format nil))))

(use-package multi-vterm
  :commands (multi-vterm
             multi-vterm-dedicated-toggle
             multi-vterm-dedicated-open)
  :config
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "M-t") 'multi-vterm-dedicated-toggle))

(general-define-key
 :states 'normal
 "M-t" 'multi-vterm-dedicated-toggle)

(aa/leader-key-def
  "te" '(evil-collection-vterm-toggle-send-escape :which-key "toggle escape in vterm"))

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-auto t)
  (corfu-echo-documentation t)
  (corfu-auto-prefix 1)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package cape-yasnippet
  :straight (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet")
  :after cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

(use-package kind-icon
  :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))

(use-package eglot
  :commands (eglot eglot-ensure)
  ;; :hook ((elixir-mode ruby-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve"))))

;; Helps with monorepo project where projects might not be the top level
;; (add-hook 'project-find-functions 'aa/find-mix-project nil nil)
;; (add-hook 'project-find-functions 'aa/find-rails-project nil nil)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (flycheck-set-indication-mode 'left-margin)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-inline
  :hook (prog-mode . flycheck-inline-mode))

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
  :hook (dired-mode . all-the-icons-dired-mode)
  :after all-the-icons
  :init
  (setq all-the-icons-dired-monochrome nil))

(use-package rg
  :commands rg)

(aa/leader-key-def
  "s" '(rg-project :which-key "search"))

(evil-collection-define-key 'normal 'rg-mode-map
  "?" 'rg-menu)

(defun google (text)
  "Search the text in google using default browser"
  (interactive (list (read-from-minibuffer
                      "Search: "
                      (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (thing-at-point 'word)))))
  (browse-url (format "https://google.com/search?q=%s" (url-hexify-string text))))

(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 48))

(defvar aa/font "JetBrains Mono")
(defun aa/apply-fonts (frame)
  "Apply selected fonts to emacs."

  ;; Set the font face based on platform
  (set-face-attribute 'default frame
                      :font aa/font
                      :weight 'regular
                      :height 150)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch frame
                      :font aa/font
                      :weight 'regular
                      :height 150)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch frame
                      :font aa/font
                      :height 150
                      :weight 'regular))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (aa/apply-fonts frame))))

(aa/apply-fonts nil)

(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(global-auto-composition-mode -1)

(defun aa/enable-auto-composition ()
  (auto-composition-mode))

(add-hook 'prog-mode-hook #'aa/enable-auto-composition)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-nord t))

(use-package doom-modeline
  :demand
  :config
  (doom-modeline-mode 1))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
