(use-package evil
  :init
  (evil-mode)
  :config ;; Execute code After a package is loaded
  (evil-set-initial-state 'vterm-mode 'emacs) ;; Set initial state in vterm terminal to insert mode
  :custom ;; Customization of package custom variables
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-redo) ;; C-r to redo
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
  :bind
  (:map evil-motion-state-map
        ("SPC" . nil)
        ("RET" . nil)
        ("TAB" . nil)))

(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(consult
                                    corfu
                                    dired
                                    elpaca
                                    ibuffer
                                    magit
                                    vertico))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(provide 'feat-evil)
