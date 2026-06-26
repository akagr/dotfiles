(use-package exec-path-from-shell
  :custom
  ;; Use a non-interactive login shell. config.fish sets up PATH for login
  ;; shells too, so this yields the same PATH as `-l -i' while skipping the
  ;; slower interactive setup (fzf/zoxide init, etc.).
  (exec-path-from-shell-arguments '("-l"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'feat-exec-path)
