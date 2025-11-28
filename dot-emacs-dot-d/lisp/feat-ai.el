(use-package monet
  :ensure (:type git :host github :repo "stevemolitor/monet" :branch "main" :depth 1
                 :files ("*.el")))

(use-package claude-code
  :after monet
  :ensure (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                 :files ("*.el" (:exclude "images/*")))
  :custom
  (claude-code-terminal-backend 'vterm)

  :config
  ;; Override the transient key bindings to use Return instead of 'y' and Escape instead of 'n'
  (with-eval-after-load 'claude-code
    (transient-replace-suffix 'claude-code-transient "y" '("<return>" "Send <return>" claude-code-send-return))
    (transient-replace-suffix 'claude-code-transient "n" '("<escape>" "Send <escape>" claude-code-send-escape)))

  ;; Use claude in a static window on the right that doesn't replace any others.
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)))

  ;; Start IDE integration with monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))

(provide 'feat-ai)
