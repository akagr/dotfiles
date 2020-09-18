					; Install theme and font
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode)

(use-package nord-theme
  :demand t
  :config
  (load-theme 'nord t))

;; Set font
(set-face-attribute 'default nil
		    :family "FiraCode Nerd Font"
		    :height 130
		    :weight 'normal)

;; Load theme after loading frame - Works with server client
(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (with-selected-frame frame
	      (load-theme 'nord t))))

(provide 'gui)
