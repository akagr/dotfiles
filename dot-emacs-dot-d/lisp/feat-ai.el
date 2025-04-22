(use-package gptel
  :ensure (:host github :repo "karthink/gptel")
  :custom
  (gptel-model 'gpt-4.1)

  :hook (gptel-mode . visual-line-mode)

  :commands (gptel gptel-send gptel-mode gptel-menu))

(global-set-key (kbd "C-c RET") #'gptel-send)

(provide 'feat-ai)
