(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :hook ((c-ts-mode c++-ts-mode ;; Autostart lsp servers for a given mode
                 lua-ts-mode
                 go-ts-mode
                 elixir-ts-mode
                 terraform-mode
                 typescript-ts-mode)
         . eglot-ensure)
  :custom
  ;; Good default
  (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
  (eglot-sync-connect nil) ;; Do not block emacs when connecting to lsp
  (eglot-autoshutdown t) ;; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable lsp server logs (Don't show lsp messages at the bottom, java)
  ;; Manual lsp servers
  :config
  (setq aa/custom-eglot-servers '((lua-mode . ("/opt/homebrew/bin/lua-language-server" "-lsp"))
                                  ((elixir-mode elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))
  (dolist (server aa/custom-eglot-servers)
    (add-to-list 'eglot-server-programs server))

  (setq-default eglot-workspace-configuration
                '((:yaml . (:schemas (:kubernetes "/*")
                                     :schemaStore (:enable t)))))

  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(start/leader-keys
  "e" '(:ignore t :wk "Eglot")
  "e e" '(eglot-reconnect :wk "Eglot Reconnect")
  "e f" '(eglot-format :wk "Eglot Format")
  "e l" '(consult-flymake :wk "Consult Flymake")
  "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
  "e r" '(eval-region :wk "Evaluate elisp in region"))

(provide 'feat-lsp)
