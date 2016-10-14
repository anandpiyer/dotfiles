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

;; prefer utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; setup temporary directory
(setq temporary-file-directory (concat user-emacs-directory "tmp"))

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
(require 'desktop)
(desktop-save-mode t)
(setq desktop-auto-save-timeout 60)

(provide 'init-core)

;;; init-core.el ends here
