;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Each set of related packages should ideally go into their own file(s).
;; This keeps the configuration digestable without having to resort to
;; literate programming for legibility

;; load package manager first...
(require 'feat-elpaca)

;; ...and everything else after that
(require 'feat-defaults)
(require 'feat-appearance)
(require 'feat-modeline)
(require 'feat-evil)
(require 'feat-keybindings)
(require 'feat-lsp)
(require 'feat-org)
(require 'feat-term)
(require 'feat-git)
(require 'feat-completion)
(require 'feat-menu-completion)
(require 'feat-which-key)
(require 'feat-search)
(require 'feat-treesitter)
(require 'feat-smartparens)
(require 'feat-helpful)
(require 'feat-templates)
(require 'feat-ai)
(require 'feat-embark)
;; major modes
(require 'feat-lua)
(require 'feat-terraform)
(require 'feat-yaml)
(require 'feat-dockerfile)
(require 'feat-typescript)
(require 'feat-markdown)
(require 'feat-json)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot edit-indirect)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
