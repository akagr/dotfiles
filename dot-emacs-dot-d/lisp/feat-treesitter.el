(use-package treesit
  :ensure nil)

(use-package treesit-auto
  :after treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)

  ;; --- Performance fix -------------------------------------------------------
  ;; treesit-auto advises `set-auto-mode-0' (i.e. runs on EVERY file open) and
  ;; rebuilds `major-mode-remap-alist' from scratch each time. Building it probes
  ;; every grammar via `treesit-ready-p', which is NOT cached, so with ~50
  ;; installed grammars this adds ~0.3-1s of latency to every single file open.
  ;;
  ;; Installed grammars don't change during a session, so we cache the computed
  ;; remap alist and reuse it. This keeps tree-sitter working for all languages
  ;; while reducing the per-open cost from ~1s to ~0.02s. The cache is rebuilt
  ;; whenever a new grammar is installed, and warmed during idle time so even the
  ;; first interactive file open is fast.
  (defvar aa/treesit-auto--remap-cache nil
    "Session cache of the `major-mode-remap-alist' built by treesit-auto.")

  (defun aa/treesit-auto--ensure-cache ()
    "Build the remap cache once per session (after a grammar change)."
    (unless aa/treesit-auto--remap-cache
      (setq aa/treesit-auto--remap-cache
            (treesit-auto--build-major-mode-remap-alist))))

  (defun aa/treesit-auto--cached-remap (&rest _)
    "Set the buffer-local remap alist from the session cache.
Drop-in replacement for `treesit-auto--set-major-remap' that avoids
re-probing every grammar on each file open."
    (aa/treesit-auto--ensure-cache)
    (setq-local major-mode-remap-alist aa/treesit-auto--remap-cache))

  (defun aa/treesit-auto-refresh-remap-cache (&rest _)
    "Invalidate the cached remap alist; rebuilt lazily on next file open."
    (interactive)
    (setq aa/treesit-auto--remap-cache nil))

  ;; Swap treesit-auto's per-open advice for the cached version.
  (advice-remove #'set-auto-mode-0 #'treesit-auto--set-major-remap)
  (advice-add #'set-auto-mode-0 :before #'aa/treesit-auto--cached-remap)

  ;; Keep the cache correct when grammars are (un)installed.
  (advice-add #'treesit-install-language-grammar :after
              #'aa/treesit-auto-refresh-remap-cache)

  ;; Warm the cache during idle so the first real file open isn't slow.
  (run-with-idle-timer 1 nil #'aa/treesit-auto--ensure-cache))

(provide 'feat-treesitter)
