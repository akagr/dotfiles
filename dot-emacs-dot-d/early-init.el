(defvar comp-deferred-compilation)
(setq comp-deferred-compilation t)

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t)

;; max memory available for gc on startup
(defvar aa/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold aa/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun aa/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun aa/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold aa/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'aa/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'aa/restore-garbage-collection-h)
(setq garbage-collection-messages nil)

(defvar aa/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist aa/-file-name-handler-alist)))

(setq site-run-file nil)

(setq inhibit-compacting-font-caches t)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t)

(setq debug-on-error t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq vc-follow-symlinks 'ask) ; restore default

(require 'straight-x)

(straight-use-package 'use-package)

(use-package gcmh
  :demand t
  :config (gcmh-mode 1))

(provide 'early-init)
