(use-package agent-vterm
  :ensure (agent-vterm :host github :repo "akagr/agent-vterm.el")
  :after vterm
  :custom
  (agent-vterm-commands
   '(("claude"  . "claude")
     ("copilot" . "copilot")))
  (agent-vterm-directory-function #'agent-vterm-project-directory)
  (agent-vterm-buffer-name-function #'agent-vterm-default-buffer-name)
  (agent-vterm-display-action
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 0.5)))
  (agent-vterm-kill-buffer-on-exit t)
  :config
  (agent-vterm-define-commands))

(start/leader-keys
  "c" '(:ignore t :wk "Agent")
  "c c" '(agent-vterm :wk "Pick agent")
  "c l" '(agent-vterm-claude :wk "Claude")
  "c g" '(agent-vterm-copilot :wk "Copilot")
  "c f" '(agent-vterm-send-dwim :wk "Send file/region"))

(provide 'feat-ai)
