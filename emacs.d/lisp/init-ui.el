;;; Init-ui.el --- UI related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default use-dialog-box nil
              ring-bell-function #'ignore
              visible-bell nil

              window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)

(add-hook 'emacs-startup-hook #'window-divider-mode)

;; Don't show the ugly stuff.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; Make mouse less jumpy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
     
;; Enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Winner mode for quick window configurations.
(when (fboundp 'winner-mode) (winner-mode 1))

;; Allow switching between active windows using Shift + arrow keys.
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; Set default font
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

;; turn off annoying splash screen and message
;; (setq inhibit-startup-screen t
;;       inhibit-splash-screen t
;;       inhibit-startup-echo-area-message t
;;       inhibit-startup-message t
;;       use-dialog-box nil)

;; Rainbow delimiters to manage delimiter explosion.
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

;; All the icons.
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts))

;; Interactively show available commands
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-min-display-lines 5
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'emacs-startup-hook #'which-key-mode))

;; Theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(setq custom-safe-themes t)

(use-package seoul256-theme
    :config
    (setq seoul256-background 236
          seoul256-alternate-background 252)
    (load-theme 'seoul256 t)
    (with-eval-after-load "seoul256-theme"
      (custom-theme-set-faces
       'seoul256
       '(font-lock-comment-delimiter-face ((t (:foreground "gray45"))))
       '(font-lock-comment-face ((t (:foreground "gray45"))))
       '(font-lock-doc-face ((t (:foreground "gray70"))))
       '(linum ((t (:foreground "gray37"))))

       `(git-gutter:added ((t (:background "darkgreen"))))
       `(git-gutter:deleted ((t (:background "darkred"))))
       `(git-gutter:modified ((t (:background "orange3"))))
       
       ;; strike through unmatched parenthesis
       '(rainbow-delimiters-unmatched-face ((t (:foreground "red"
                                                :inherit unspecified
                                                :strike-through t)))))))

;; (use-package zenburn-theme
;;    :config
;;    (load-theme 'zenburn t)

;;    (with-eval-after-load "zenburn-theme"
;;      (custom-theme-set-faces
;;       'zenburn

;;       '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
;;       '(font-lock-comment-face ((t (:foreground "gray55"))))
;;       '(font-lock-doc-face ((t (:foreground "gray70"))))
;;       '(shm-current-face ((t (:background "gray27"))))
;;       '(linum ((t (:foreground "gray37"))))
;;       '(fringe ((t (:background "#3f3f3f"))))
;;       ;'(vhl/default-face ((t (:background "gray27"))))
;;       ;'(vertical-border ((t (:foreground "red"))))
      
;;       ;; strike through unmatched parenthesis
;;       '(rainbow-delimiters-unmatched-face ((t (:foreground "red"
;;                                                :inherit unspecified
;;                                                :strike-through t)))))))

;; Mode line.
(use-package smart-mode-line
  :config
  (progn (setq sml/no-confirm-load-theme t
               sml/theme 'nil)
         (sml/setup)))

(provide 'init-ui)

;;; init-ui.el ends here
