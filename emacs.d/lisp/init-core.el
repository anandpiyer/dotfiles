;;; core.el --- Core Emacs stuff
;;; commentary:
;;; code:

;; garbage collection
;;(defun my-minibuffer-setup-hook () (setq gc-cons-threshold most-positive-fixnum))
;;(defun my-minibuffer-exit-hook () (setq gc-cons-threshold (* 64 1024 1024)))
;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;;(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; prefer utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; backup
(setq backup-directory-alist
      `((".*" . ,user-emacs-temp-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-emacs-temp-directory t)))
(setq make-backup-files nil
      backup-by-copying t
      version-control t
      delete-old-versions t
      create-lockfiles nil
      ring-bell-function 'ignore)

;; recent files
(use-package recentf
  :ensure nil
  :defer t
  :init
  (progn
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    (setq recentf-save-file (concat user-emacs-cache-directory "recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500
          recentf-auto-cleanup 'never))
  :config
  (progn
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude (expand-file-name user-emacs-temp-directory))
    (add-to-list 'recentf-exclude
                   (expand-file-name user-emacs-cache-directory))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

;; save sessions
(use-package desktop
  :ensure nil
  :init
  (progn
    (desktop-save-mode 1)
    (setq desktop-auto-save-timeout 60
          desktop-globals-to-save
          (append '((file-name-history . 100)
                    (compile-history . 100)
                    (command-history . 100)
                    (extended-command-history . 100)
                    (shell-command-history . 100)
                    (query-replace-history . 100)
                    (regexp-history . 100)
                    (grep-history . 100)
                    (minibuffer-history . 100))
                  desktop-globals-to-save))))

(provide 'init-core)

;;; init-core.el ends here
