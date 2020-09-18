(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(add-to-list 'load-path "~/.emacs.d/modules")
(setq custom-file "~/.emacs.d/modules/custom-file.el")
(load-file custom-file)

;;------------------- Package Config -----------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)


;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;;------------------- Packages ------------------------------
(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode)
  (evil-set-leader 'normal (kbd ","))
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>oe") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
  (evil-define-key 'normal 'global (kbd "<leader>oo") (lambda() (interactive)(find-file "~/Dropbox/akash.org")))
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package hydra)

(use-package flx)

(use-package counsel
  :after hydra flx
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (evil-define-key 'normal 'global (kbd "C-b") 'ivy-switch-buffer)
  :bind
  ("M-x" . counsel-M-x)
  ("C-s" . swiper-isearch))

(use-package ivy-rich
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

(use-package magit
  :after counsel evil
  :demand t
  :bind
  ("C-x g" . magit-status)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status))

(use-package libgit)

(use-package magit-libgit
  :after (magit libgit))

(use-package evil-magit
  :demand t
  :after magit evil)

(use-package projectile
  :after evil
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "C-p") 'projectile-find-file)
  (projectile-mode +1))

(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode 1))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook
  (ruby-mode . lsp-deferred)
  (elixir-mode . lsp-deferred)
  :init
  (add-to-list 'exec-path "/Users/akash/Downloads/elixir-ls")
  (setq lsp-completion-provider :capf))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-doc-max-height 200)
  :after lsp-mode)

(use-package company-lsp
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

(use-package flycheck
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

(use-package elixir-mode)

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :after evil
  :demand t
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))

  :config
  (evil-define-key 'normal 'global (kbd "<leader>n") 'dired-sidebar-toggle-sidebar)
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-subtree-line-prefix "-")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; Run M-x all-the-icons-install-fonts to install the icons
(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package yaml-mode)

(use-package ripgrep)

(use-package deadgrep
  :bind (("C-c h" . #'deadgrep)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom (which-key-idle-delay 1.0))

(load "gui")

;;------------------- Better Defaults ------------------------

(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 inhibit-startup-message t
 load-prefer-newer t
 ring-bell-function 'ignore
 sentence-end-double-space nil
 truncate-lines 1
 vc-follow-symlinks nil)

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

;; Disable windows chrome
(progn
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1))

(global-so-long-mode)
(electric-pair-mode)
(show-paren-mode)
(column-number-mode)
(global-display-line-numbers-mode)

;; Make the command key behave as 'meta'
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

(add-to-list 'default-frame-alist '(width . 130))
(add-to-list 'default-frame-alist '(height . 48))

;; Delete whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))
