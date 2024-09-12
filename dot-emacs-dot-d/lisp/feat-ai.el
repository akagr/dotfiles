(use-package gptel
  :custom
  (gptel-model "gpt-4o")

  :hook (gptel-mode . visual-line-mode)

  :commands (gptel gptel-send gptel-mode gptel-menu))

(global-set-key (kbd "C-c RET") #'gptel-send)

(provide 'feat-ai)
