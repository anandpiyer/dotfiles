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
(setq scroll-conservatively 100000
      scroll-preserve-screen-position t
      scroll-margin 3)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Interactively show available commands
(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-min-display-lines 3)
  (which-key-mode t))

;; Set default UI font
(set-face-attribute 'default nil
                    :family "Input"
                    :height 140
                    :weight 'normal
                    :width 'normal)

; keep modeline tidy
(require 'diminish)

;; powerline
(use-package powerline
  :ensure powerline-evil
  :init
  (powerline-evil-vim-color-theme))
(add-hook 'after-init-hook 'powerline-reset)

;(use-package gruvbox-theme
;  :config
;  (load-theme 'gruvbox t))

;(use-package leuven-theme
;  :config
;  (load-theme 'leuven t))
(use-package moe-theme
  :config
  (moe-light))
;  (powerline-moe-theme))
;(require 'moe-theme-switcher)

(use-package smart-mode-line
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          ;; delegate theming to the currently active theme
          sml/theme nil)
    (add-hook 'after-init-hook #'sml/setup)))

(provide 'init-ui)
;;; init-ui.el ends here
