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
  :custom
  (claude-code-terminal-backend 'vterm)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*claude"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 90)))
  (claude-code-mode))

(provide 'feat-ai)
