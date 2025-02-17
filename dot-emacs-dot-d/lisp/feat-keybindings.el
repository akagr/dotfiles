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
    "d" '(dired-jump :wk "Dired"))

  (start/leader-keys
    "e" '(:ignore t :wk "Eglot")
    "e e" '(eglot-reconnect :wk "Eglot Reconnect")
    "e f" '(eglot-format :wk "Eglot Format")
    "e l" '(consult-flymake :wk "Consult Flymake")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (start/leader-keys
    "g" '(:ignore t :wk "Git")
    "g g" '(magit-status :wk "Status")
    "g b" '(magit-blame :wk "Blame"))

  (start/leader-keys
    "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h r" '((lambda () (interactive)
              (load-file "~/.emacs.d/init.el"))
            :wk "Reload Emacs config"))

  (start/leader-keys
    "o" '(:ignore t :wk "Org")
    "o o" '(org-roam-node-find :wk "Find node")
    "o i" '(org-roam-node-insert :wk "Insert node")
    "o s" '(org-roam-db-sync :wk "Sync DB"))

  (start/leader-keys
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "Switch to project")
    "p f" '(project-find-file :wk "Project find file")
    "p k" '(project-kill-buffers :wk "Kill project buffers"))

  (start/leader-keys
    "v" '(vterm :wk "Vterm"))

  (start/leader-keys
    "S" '(rg-project :wk "Search project"))

  (start/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers"))

  (start/leader-keys
    "w" '(:ignore t :wk "Window")
    "w w" '(split-window-horizontally :wk "Split |")
    "w s" '(split-window-vertically :wk "Split --")
    "w d" '(delete-window :wk "Delete")))

(provide 'feat-keybindings)
