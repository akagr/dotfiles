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
  "ee" '((lambda () (interactive) (find-file (expand-file-name "config.org" user-emacs-directory))) :which-key "open config")
  "er" '((lambda () (interactive) (load-file user-init-file)) :which-key "reload config")
  "ek" '(kill-emacs :which-key "kill emacs")

  "h"  '(help-command :which-key "help")

  "t"  '(:ignore t :which-key "toggle")
  "tt" '(load-theme :which-key "theme")
  "tw" '(toggle-truncate-lines :which-key "wrap lines")

  "c"  '(:ignore t :which-key "code")
  "ca" '(align-regexp :which-key "align regex")
  "cc" '(xref-find-definitions :which-key "find definitions")
  "ce" '(eval-last-sexp :which-key "eval last sexp")
  "cr" '(xref-find-references :which-key "find references"))

(defun aa/apply-fonts (frame)
  "Apply selected fonts to emacs."

  ;; Set the font face based on platform
  (set-face-attribute 'default frame
                      :font "JetBrains Mono"
                      :weight 'regular
                      :height 150)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch frame
                      :font "JetBrains Mono"
                      :weight 'regular
                      :height 150)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch frame
                      :font "JetBrains Mono"
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

(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 48))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(custom-set-faces '(mode-line ((t (:box (:line-width 1 :color "gray50"))))))

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
  (evil-collection-init
   '(deadgrep
     dired
     dired-sidebar
     ediff
     embark
     grep
     ibuffer
     magit
     occur
     vterm
     wgrep
     xref)))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'init-org)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (org-align-tags t))
                      nil t)))

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
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ;; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(aa/leader-key-def
"b" '(:ignore t :which-key "buffer")
"bb" '(switch-to-buffer :which-key "list buffers")
"bc" '(kill-this-buffer :which-key "kill current")
"bd" '(aa/close-and-kill-this-pane :which-key "close current")
"bp" '(projectile-switch-to-buffer :which-key "list in project"))

(general-define-key
 :states 'normal
 "C-s" 'consult-line)

(use-package libgit)

(use-package magit
  :commands (magit magit-status magit-blame))

(use-package magit-libgit
  :after (magit libgit))

(aa/leader-key-def
  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "status")
  "gb" '(magit-blame :which-key "blame"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package perspective
  :init
  (persp-mode))

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
  :diminish
  :init
  (setq projectile-enable-caching t)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1))

(aa/leader-key-def
  "p"  '(projectile-command-map :which-key "projectile"))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode)
  :diminish)

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package elixir-mode
  :mode "\\.exs?\\'"
  :config
  ;; Activate ruby end mode on elixir. This helps auto-inserting
  ;; ruby style 'end' after writing 'do' in elixir.
  (add-hook 'elixir-mode-hook
            (lambda ()
              (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                   "\\(?:^\\|\\s-+\\)\\(?:do\\)")
              (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
              (ruby-end-mode +1)))
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package kubel
  :commands kubel)

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

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package evil-paredit
  :hook (emacs-lisp-mode . evil-paredit-mode))

(use-package banner-comment
  :commands banner-comment)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package vterm
  :commands (vterm))

(use-package vterm-toggle
  :after vterm
  :commands (vterm-toggle)
  :config
  (setq vterm-toggle-hide-method 'reset-window-configration)
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-t") 'vterm-toggle))
(general-define-key
 :states 'normal
 "C-t" 'vterm-toggle)

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :init
  (setq company-idle-delay 0)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1))

(use-package eglot
  :after elixir-mode
  :hook ((elixir-mode ruby-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "/Users/akash/Downloads/elixir-ls/language_server.sh")))

;; Helps with monorepo project where projects might not be the top level
(add-hook 'project-find-functions 'aa/find-mix-project nil nil)
(add-hook 'project-find-functions 'aa/find-rails-project nil nil)

(use-package flycheck
  :diminish
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

(use-package ripgrep)

(use-package deadgrep
  :commands deadgrep)

(aa/leader-key-def
  "s" '(deadgrep :which-key "search"))

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

(defun aa/dashcase (str)
  "Converts a string to dash case.

   Example:
   (aa/dashcase \"Hello World\")
   => \"hello-world\" "
  (let ((down (downcase str)))
    (replace-regexp-in-string "\\([^A-Za-z]\\)" "-" down)))
