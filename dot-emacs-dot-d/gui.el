(provide 'gui)
(if (display-graphic-p)
    (use-package fira-code-mode
    :ensure t
    :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
    :hook prog-mode)
  ())

(if (display-graphic-p)
    (use-package nord-theme
    :ensure t
    :config
    (load-theme 'nord t))
  ())

(if (display-graphic-p)
    (set-face-attribute 'default nil
			:family "FiraCode Nerd Font"
			:height 130
			:weight 'normal)
  ())

(if (display-graphic-p)
    ;; Load theme after loading frame - Works with server client
    (if (daemonp)
	(add-hook 'after-make-frame-functions
	    (lambda (frame)
		(with-selected-frame frame
		    (load-theme 'nord t))))
      (load-theme 'nord t))
  ())
