;; Check for minimum emacs version.
;; The idea is to support the latest stable version at all times.
(defvar aa/emacs-minimum-version 29)
(unless (>= emacs-major-version aa/emacs-minimum-version)
  (error (format "Emacs version found is: %d. Minimum supported version is: %d."
                 emacs-major-version
                 aa/emacs-minimum-version)))

;; The GC threshold is raised to its maximum in `early-init.el' for the whole
;; of startup. It is restored to a sane runtime value below via
;; `emacs-startup-hook'.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Each set of related packages should ideally go into their own file(s).
;; This keeps the configuration digestable without having to resort to
;; literate programming for legibility

;; load package manager first...
(require 'feat-elpaca)

;; ...and everything else after that
(require 'feat-defaults)
(require 'feat-exec-path)
(require 'feat-buffer)
(require 'feat-appearance)
(require 'feat-modeline)
(require 'feat-evil)
;; feat-keybindings sets up `general' and the `start/leader-keys' definer, then
;; calls `elpaca-wait' so they are available synchronously. Any feature file
;; that attaches leader bindings must be required AFTER this line.
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
(require 'feat-dired)
(require 'feat-windows)
(require 'feat-layout-restore)
(require 'feat-http)
;; major modes
(require 'feat-dockerfile)
(require 'feat-elixir)
(require 'feat-go)
(require 'feat-lua)
(require 'feat-markdown)
(require 'feat-terraform)
(require 'feat-typescript)
(require 'feat-yaml)

;; Restore the GC threshold after startup. A generous value (100 MB) keeps GC
;; pauses rare during normal editing; modern packages (eglot, corfu, vertico)
;; allocate heavily, so the old 2 MB value caused frequent stutters.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1000 1000)
                  gc-cons-percentage 0.1)))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
