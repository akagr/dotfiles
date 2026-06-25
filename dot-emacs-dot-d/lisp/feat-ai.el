(use-package agent-vterm
  :ensure (agent-vterm :host github :repo "akagr/agent-vterm.el")
  :after vterm
  :bind (("C-c a a" . agent-vterm)
         ("C-c a c" . agent-vterm-claude)
         ("C-c a g" . agent-vterm-copilot))
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

(provide 'feat-ai)
