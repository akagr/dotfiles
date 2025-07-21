(use-package gptel
  :ensure (:host github :repo "karthink/gptel")
  :custom
  (gptel-model 'gpt-4.1)
  :hook (gptel-mode . visual-line-mode)

  :commands (gptel gptel-send gptel-mode gptel-menu))

(global-set-key (kbd "C-c RET") #'gptel-send)

(use-package claude-code
  :ensure (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                 :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode))

(provide 'feat-ai)
