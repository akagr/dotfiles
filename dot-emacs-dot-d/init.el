;; Do not use `init.el` for `custom-*` code - use `custom-file.el`.
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;;------------------- Package Config -----------------------
(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
  (evil-mode))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode)

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

;;------------------- Better Defaults ------------------------

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 130 :weight 'normal)

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

(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

 ;; Disable windows chrome
(progn
 (scroll-bar-mode -1)
 (tool-bar-mode -1))

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

;; Load theme after loading frame - Works with server client
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (load-theme 'nord t))))
    (load-theme 'nord t))
