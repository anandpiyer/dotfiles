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

;; Save and retrieve window layouts, a simpler alternative to registers.
(use-package eyebrowse
  :init
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-left-delimiter " "
        eyebrowse-mode-line-right-delimiter " "
        eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode))

;; Visually select windows.
(use-package ace-window
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

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

;; (use-package seoul256-theme
;;     :config
;;     (setq seoul256-background 236
;;           seoul256-alternate-background 252)
;;     (load-theme 'seoul256 t)
;;     (with-eval-after-load "seoul256-theme"
;;       (custom-theme-set-faces
;;        'seoul256
;;        '(font-lock-comment-delimiter-face ((t (:foreground "gray45"))))
;;        '(font-lock-comment-face ((t (:foreground "gray45"))))
;;        '(font-lock-doc-face ((t (:foreground "gray70"))))
;;        '(linum ((t (:foreground "gray37"))))

;;        `(git-gutter:added ((t (:background "darkgreen"))))
;;        `(git-gutter:deleted ((t (:background "darkred"))))
;;        `(git-gutter:modified ((t (:background "orange3"))))
       
;;        ;; strike through unmatched parenthesis
;;        '(rainbow-delimiters-unmatched-face ((t (:foreground "red"
;;                                                 :inherit unspecified
;;                                                 :strike-through t)))))))

(use-package zenburn-theme
   :config
   (load-theme 'zenburn t)

   (with-eval-after-load "zenburn-theme"
     (custom-theme-set-faces
      'zenburn

      '(region ((t (:background "#007475"))))
      '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
      '(font-lock-comment-face ((t (:foreground "gray55"))))
      '(font-lock-doc-face ((t (:foreground "gray70"))))
      '(shm-current-face ((t (:background "gray27"))))
      '(linum ((t (:foreground "gray37"))))
      '(fringe ((t (:background "#3f3f3f"))))

      ;; eyebrowse
      `(eyebrowse-mode-line-active ((t (:foreground "#F0DFAF"))))
      `(eyebrowse-mode-line-inactive ((t (:foreground "gray37"))))

      ;; ace-window
      `(aw-leading-char-face ((t (:foreground "#F0DFAF" :weight bold :height 4.0))))
        
      ;; strike through unmatched parenthesis
      '(rainbow-delimiters-unmatched-face ((t (:foreground "red"
                                               :inherit unspecified
                                               :strike-through t)))))))

;; Mode line.
;; (use-package smart-mode-line
;;   :config
;;   (setq sml/no-confirm-load-theme t
;;         sml/theme 'nil)
;;   (add-hook 'after-init-hook #'sml/setup))

;; Telephone line.
(use-package telephone-line
  :after evil
  :init
  (require 'telephone-line-config)

  ;; Bunch of mode line segments. Most of them taken from:
  ;; https://github.com/domtronn/all-the-icons.el/wiki/Spaceline

  ;; Shows projectile information if available.
  (telephone-line-defsegment api--projectile-segment ()
    (if (and (fboundp 'projectile-project-name)
             (projectile-project-name))
        (propertize (format " [%s] " (concat (projectile-project-name)))
                    'face '(:inherit)
                    'display '(raise 0.0)
                    'help-echo "Switch Project"
                    'mouse-face '(:box 1)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 (lambda ()
                                           (interactive)
                                           (projectile-switch-project))))
      (propertize "×" 'face '(:inherit))))
  
  (defun api--github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (all-the-icons-alltheicon "git")
                   'face '(:height 1.1 :inherit)
                   'display '(raise 0.0))
       (propertize " · ")
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:family ,(all-the-icons-octicon-family)
                                   :height 1.1 :inherit)
                   'display '(raise 0.0))
       (propertize (format " %s" branch)
                   'face `(:inherit)
                   'display '(raise 0.0)))))

  ;; Shows version control and branch.
  (telephone-line-defsegment api--vc-segment ()
    (when vc-mode
      (cond ((string-match "Git[:-]" vc-mode) (api--github-vc))
            (t (propertize (format "%s" vc-mode))))))

  ;; Shows all-the-icon icon for the current major mode.
  (telephone-line-defsegment api--mode-icon-segment ()
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (propertize (format "%s " icon)
                    'help-echo (format "Major-mode: `%s`" major-mode)
                    'display '(raise 0.0)
                    'face `(:height 1.1
                                    :family ,(all-the-icons-icon-family-for-buffer)
                                    :inherit)))))

  ;; Shows the current buffer id. If projectile-mode is active, shows the
  ;; relative path to the file from the project root.
  (telephone-line-defsegment api--buffer-id-segment () 
    (if (fboundp 'projectile-project-root)
        (let* ((buf (or (buffer-file-name) (buffer-name)))
               (proj (ignore-errors (projectile-project-root)) )
               (name (if (buffer-file-name)
                         (or (cadr (split-string buf proj))
                             (format-mode-line "%b"))
                       (format-mode-line "%b"))))
          (propertize (format "%s" name)
                      'face `(:inherit)
                      'display '(raise 0.0)
                      'help-echo (format "Major-mode: `%s`" major-mode)))
      (propertize (format-mode-line "%b ")
                  'face '(:inherit)
                  'display '(raise 0.0))))

  ;; Shows status of the current buffer using all-the-icons.
  (telephone-line-defsegment api--buffer-modified-segment ()
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon
               "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon
               "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon
               "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))

      (propertize (format " %s " (apply (cadr result) (cddr result)))
                  'face `(:family ,(funcall (car result)) :inherit ))))

  ;; Shows the buffer size.
  (telephone-line-defsegment api--buffersize-segment ()
    (propertize (format-mode-line " %I ")
                'face `(:height 1.0 :inherit)
                'display '(raise 0.0)))

  ;; Shows a bunch of buffer related things.
  (telephone-line-defsegment api--buffer-segment ()
    `(""
      mode-line-mule-info
      mode-line-client
      mode-line-remote
      mode-line-frame-identification
      ,(telephone-line-raw (format-mode-line " %I "))
      ,(telephone-line-raw mode-line-buffer-identification t)))

  ;; Shows the current pointer position in the buffer.
  (telephone-line-defsegment api--position-segment ()
    (if (eq major-mode 'paradox-menu-mode)
        (telephone-line-trim (format-mode-line mode-line-front-space))
      '(" %3l:%2c %p ")))
  
  (setq telephone-line-height 25)
  
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (api--vc-segment
                     telephone-line-process-segment))
          (nil    . (api--projectile-segment
                     api--buffer-segment
                     api--buffer-modified-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (api--mode-icon-segment
                     telephone-line-major-mode-segment))
          (evil   . (api--position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (telephone-line-mode t))

(provide 'init-ui)

;;; init-ui.el ends here
