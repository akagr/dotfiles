;; Installing straight from github because ef-themes don't
;; seem to be available on elpa.
(elpaca (ef-themes
         :host github
         :repo "protesilaos/ef-themes")
  (progn
    (setq ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui t)
    (ef-themes-select 'ef-owl)))

;; need to run (nerd-icons-install-fonts) mannually once after installing
(use-package nerd-icons
  :if (display-graphic-p))
(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; font
(defvar aa/font "JetBrainsMono Nerd Font")
(defun aa/apply-fonts (frame)
  "Apply selected fonts to emacs."

  ;; Set the font face based on platform
  (set-face-attribute 'default frame
                      :font aa/font
                      :weight 'regular
                      :height 150)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch frame
                      :font aa/font
                      :weight 'regular
                      :height 150)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch frame
                      :font aa/font
                      :height 150
                      :weight 'regular))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (aa/apply-fonts frame))))

(aa/apply-fonts nil)

;; colorful delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make titlebar same colour as the theme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(provide 'feat-appearance)
