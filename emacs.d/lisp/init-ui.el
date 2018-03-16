;;; Init-ui.el --- UI related settings
;;; Commentary:
;;; Code:

;; don't show the ugly stuff
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; turn off annoying splash screen and message
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      use-dialog-box nil)

;; better scrolling
(setq scroll-conservatively 100000
      scroll-preserve-screen-position t
      scroll-margin 3)

;; make mouse less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
     
;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Interactively show available commands
(use-package which-key
  :diminish which-key-mode
  :config
  (progn (setq which-key-idle-delay 0.5)
         (setq which-key-min-display-lines 3)
         (which-key-mode t)))

;; Set default UI font
;; (set-face-attribute 'default nil
;;                     :family "Input Mono Narrow"
;;                     :height 140
;;                     :weight 'extra-light
;;                     :width 'normal)

;; (set-face-attribute 'default nil
;;                     :family "Iosevka"
;;                     :height 130
;;                     :weight 'light
;;                     :width 'normal)

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 130
                    :weight 'normal
                    :width 'normal)


(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(setq custom-safe-themes t)

;; (use-package seoul256
;;    :ensure nil
;;    :init
;;    (progn (setq seoul256-background 237)
;;           (setq seoul256-alternate-background 252)
;;           (load-theme 'seoul256 t)))

;; (use-package gruvbox
;;   :init (load-theme 'gruvbox t))

;;(use-package zenburn-theme
;; :init (load-theme 'zenburn t))

(use-package nord-theme
  :init (load-theme 'nord t))

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
(setq auto-window-vscroll nil)

(provide 'init-ui)

;;; init-ui.el ends here
