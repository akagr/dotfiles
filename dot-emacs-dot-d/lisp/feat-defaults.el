(use-package emacs
  :ensure nil
  :config
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
  (global-hl-line-mode) ;; highlight current line
  (global-display-line-numbers-mode) ;; show line numbers in all buffers

  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'super))

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(provide 'feat-defaults)
