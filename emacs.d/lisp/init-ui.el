;;; Init-ui.el --- UI related settings
;;; Commentary:
;;; Code:

;; don't show the ugly stuff
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; turn off annoying splash screen and message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; better scrolling
;; (setq scroll-conservatively 100000
;;       scroll-preserve-screen-position t
;;       scroll-margin 3)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Interactively show available commands
(use-package which-key
  :diminish which-key-mode
  :config
  (progn (setq which-key-idle-delay 0.5)
         (setq which-key-min-display-lines 3)
         (which-key-mode t)))

;; Set default UI font
(set-face-attribute 'default nil
                    :family "Input"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;Powerline
(use-package powerline
  :init
  (setq powerline-default-separator 'slant))

;; (use-package powerline-evil
;;   :init
;;   (powerline-center-evil-theme)
;;   (add-hook 'after-init-hook 'powerline-reset))

;; (use-package spaceline
;; :init
;; (progn (setq spaceline-minor-modes-separator " ")
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
;; (add-hook 'after-init-hook 'powerline-reset)))

;; (use-package gruvbox-theme
;; :config
;; (load-theme 'gruvbox t))

;; (use-package smart-mode-line
;;   :config
;;   (use-package smart-mode-line-powerline-theme)
;;   (setq sml/no-confirm-load-theme t)
;;                                         ;(setq sml/theme 'respectful)
;;   (require 'smart-mode-line)
;;   (sml/setup))

(use-package moe-theme
  :config
  (progn
    (powerline-moe-theme)
    (require 'moe-theme-switcher)
    (add-hook 'after-init-hook 'powerline-reset)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'init-ui)

;;; init-ui.el ends here
