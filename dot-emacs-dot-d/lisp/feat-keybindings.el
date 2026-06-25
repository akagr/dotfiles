(use-package general
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the leader key
  (general-create-definer start/leader-keys
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"           ;; Set leader key
    :global-prefix "C-SPC") ;; Set global leader key

  (start/leader-keys
    "f" '(:ignore t :wk "Files")
    "f e" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Emacs config")
    "f r" '(consult-recent-file :wk "Recent files")
    "f f" '(find-file :wk "Find file")
    "f t" '((lambda () (interactive) (find-file tempel-path)) :wk "Templates"))

  (start/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b d" '(evil-delete-buffer :wk "Delete buffer")
    "b k" '(kill-current-buffer :wk "Kill this buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.emacs.d/init.el"))
            :wk "Reload Emacs config"))

  (start/leader-keys
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "Switch to project")
    "p f" '(project-find-file :wk "Project find file")
    "p k" '(project-kill-buffers :wk "Kill project buffers"))

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers"))

  (start/leader-keys
    "w" '(:ignore t :wk "Window")
    "w w" '(split-window-horizontally :wk "Split |")
    "w s" '(split-window-vertically :wk "Split --")
    "w d" '(delete-window :wk "Delete")))

;; Ensure `general' and the `start/leader-keys' definer above are fully
;; installed and evaluated before any later feature file attaches its own
;; package-specific leader bindings. Elpaca installs packages asynchronously,
;; so without this wait the definer would not yet exist when files such as
;; feat-git, feat-org or feat-lsp are loaded.
(elpaca-wait)

(provide 'feat-keybindings)
