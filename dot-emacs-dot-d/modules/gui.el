(provide 'gui)
(if (display-graphic-p)
    (progn

      ;; Install theme and font
      (use-package fira-code-mode
	:ensure t
	:custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
	:hook prog-mode)
      (use-package nord-theme
	:ensure t
	:config
	(load-theme 'nord t))

      ;; Set font
      (set-face-attribute 'default nil
			  :family "FiraCode Nerd Font"
			  :height 130
			  :weight 'normal)

      ;; Load theme after loading frame - Works with server client
      (if (daemonp)
	  (add-hook 'after-make-frame-functions
		    (lambda (frame)
		      (with-selected-frame frame
			(load-theme 'nord t))))
	(load-theme 'nord t)))
  ())
