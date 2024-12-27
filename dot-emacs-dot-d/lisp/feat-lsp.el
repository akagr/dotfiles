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

  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

(provide 'feat-lsp)
