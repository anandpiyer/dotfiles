;;; core.el --- Core Emacs stuff
;;; commentary:
;;; code:

;; garbage collection
(defun my-minibuffer-setup-hook () (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook () (setq gc-cons-threshold (* 64 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; start emacs server if not started.
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; UI
;; Don't show the ugly tool bar and scroll bar. Also avoid
;; the menu bar if not in GUI mode.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; turn off annoying splash screen and message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; prefer utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-by-copying t
      version-control t
      delete-old-versions t
      create-lockfiles nil
      ring-bell-function 'ignore)

;; recent files
(require 'recentf)
;(setq recentf-save-file (concat emacs-cache-directory "recentf"))
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 500)
(setq recentf-auto-cleanup 300)
(recentf-mode t)

;; save sessions
;(require 'desktop)
;(desktop-save-mode t)
;(setq desktop-auto-save-timeout 60)

(global-linum-mode t)
(global-hl-line-mode t)

(setq column-number-mode t)
(setq-default indent-tabs-mode nil)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-min-display-lines 3)
  (which-key-mode t))

(provide 'init-core)

;;; init-core.el ends here
