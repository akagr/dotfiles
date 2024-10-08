;; (use-package eglot
;;   :ensure nil ;; Don't install eglot because it's now built-in
;;   :hook ((c-mode c++-mode ;; Autostart lsp servers for a given mode
;;                  lua-mode
;;                  terraform-mode
;;                  typescript-mode)
;;          . eglot-ensure)
;;   :custom
;;   ;; Good default
;;   (eglot-events-buffer-size 0) ;; No event buffers (Lsp server logs)
;;   (eglot-autoshutdown t) ;; Shutdown unused servers.
;;   (eglot-report-progress nil) ;; Disable lsp server logs (Don't show lsp messages at the bottom, java)
;;   ;; Manual lsp servers
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(lua-mode . ("/opt/homebrew/bin/lua-language-server" "-lsp"))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :hook ((terraform-mode . lsp-deferred))
  :custom
  (setq lsp-disabled-clients '(tfls)))

(provide 'feat-lsp)
