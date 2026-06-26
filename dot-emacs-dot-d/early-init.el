;;; early-init.el --- Loaded before the GUI and package system -*- lexical-binding: t; -*-

;; Raise the GC threshold as high as possible for the duration of startup so
;; that virtually no garbage collection happens while Emacs and Elpaca load
;; packages. `init.el' restores a sane runtime value via `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; We manage packages with Elpaca, so don't let package.el activate anything.
(setq package-enable-at-startup nil)

;; Avoid a costly frame resize when the default font is applied, and skip the
;; startup screen.
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t)

;; Disable the tool-bar and scroll-bars before the first frame is drawn so they
;; are never rendered then removed (the menu-bar is intentionally kept; see
;; feat-defaults). This is cheaper than toggling the modes after startup.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't compact font caches during GC: trades a little memory for snappier
;; redisplay, which matters with icon/ligature fonts like Nerd Font.
(setq inhibit-compacting-font-caches t)

;; Keep native-compilation warnings/errors out of the way; they pop up async
;; buffers that interrupt the workflow without being actionable.
(setq native-comp-async-report-warnings-errors 'silent)
