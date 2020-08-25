;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(add-to-list 'load-path "~/.emacs.d/personal")
(setq custom-file "~/.emacs.d/personal/custom-file.el")
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
(require 'use-package)

;;------------------- Packages ------------------------------
(use-package evil
  :ensure t
  :init
  :config
  (evil-mode)
  (evil-set-leader 'normal (kbd ","))
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-buffer))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package hydra
  :ensure t)

(use-package counsel
  :ensure t
  :requires (hydra)
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("M-x" . counsel-M-x)
  ("C-s" . swiper-isearch)
  ("C-x b" . ivy-switch-buffer))

(use-package projectile
  :ensure t
  :after evil
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (projectile-mode +1))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (ruby-mode . lsp-deferred)
  (elixir-mode . lsp-deferred)
  :init
  (add-to-list 'exec-path "/Users/akash/Downloads/elixir-ls"))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode 1))

(use-package elixir-mode
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :after all-the-icons
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (evil-define-key 'normal 'global (kbd "<leader>n") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;;------------------- Better Defaults ------------------------

(setq-default
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t
 ;; Do not show the startup message.
 inhibit-startup-message t
 ;; Do not create lockfiles.
 create-lockfiles nil
 ;; Do not autosave.
 auto-save-default nil

 display-line-numbers t)

(setq vc-follow-symlinks nil) ; always follow symlinks

(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

 ;; Disable windows chrome
(progn
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1))

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

(load "gui")
