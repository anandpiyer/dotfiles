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

;; Powerline
;; (use-package powerline
;;   :init
;;   (setq powerline-default-separator 'slant))

;; (use-package powerline-evil
;;   :init
;;   (powerline-center-evil-theme)
;;   (add-hook 'after-init-hook 'powerline-reset))

;; (use-package spaceline
;;   :init
;;   (progn (setq spaceline-minor-modes-separator " ")
;;          (require 'spaceline-config)
;;          (spaceline-spacemacs-theme)
;;          (add-hook 'after-init-hook 'powerline-reset)))

(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(setq custom-safe-themes t)

(use-package seoul256
   :ensure nil
   :init
   (setq seoul256-background 237)
   (setq seoul256-background 253)
   (load-theme 'seoul256 t))

;; (use-package gruvbox-theme
;;   :config
;;   (progn (setq gruvbox-contrast 'soft)
;;          (load-theme 'gruvbox t)))

(use-package smart-mode-line
  :config
  (progn (setq sml/no-confirm-load-theme t
               sml/theme 'nil)
         (sml/setup)))

(use-package rainbow-delimiters
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;(setq ns-use-srgb-colorspace nil)

(provide 'init-ui)

;;; init-ui.el ends here
