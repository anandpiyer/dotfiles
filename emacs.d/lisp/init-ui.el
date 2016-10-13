;;; Init-ui.el --- UI related settings
;;; Commentary:
;;; Code:

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
(setq scroll-conservatively 100000
      scroll-preserve-screen-position t
      scroll-margin 3)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; hybrid relative line number
(use-package nlinum
  :config
  (global-nlinum-mode t)

  (use-package nlinum-relative
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)))

(global-hl-line-mode t)
(show-paren-mode)
(line-number-mode t)
(column-number-mode t)
(display-time-mode t)
(size-indication-mode t)

(setq show-paren-delay 0)
(setq column-number-mode t)

; use spaces instead of tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

; use a single spaces at sentence ending
(setq sentence-end-double-space nil)

; delete selections with keypress
(delete-selection-mode t)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-min-display-lines 3)
  (which-key-mode t))

;; Set default font
(set-face-attribute 'default nil
                    :family "Input"
                    :height 140
                    :weight 'normal
                    :width 'normal)

; keep modeline tidy
(require 'diminish)

;; powerline
;(use-package powerline-evil)
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
(use-package moe-theme)
  :config
  (moe-light)
;  (powerline-moe-theme))
;(require 'moe-theme-switcher)

(use-package smart-mode-line
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          ;; delegate theming to the currently active theme
          sml/theme nil)
    (add-hook 'after-init-hook #'sml/setup)))

(use-package color-identifiers-mode
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(provide 'init-ui)
;;; init-ui.el ends here
