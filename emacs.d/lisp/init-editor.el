;;; init-editor.el --- Editor related settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default fill-column 80
              word-wrap t
              sentence-end-double-space nil

              ;; use spaces, not tabs
              indent-tabs-mode nil
              tab-always-indent t
              tab-width 4

              ;; Scrolling
              hscroll-margin 1
              hscroll-step 1
              scroll-conservatively 1001
              scroll-margin 0
              scroll-preserve-screen-position t

              size-indication-mode t
              line-number-mode t
              column-number-mode t
              delete-selection-mode t)

;; Show matching paranthesis.
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'after-init-hook #'show-paren-mode)

;; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; Highlight the current line.
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Recent files
(use-package recentf
  :hook (emacs-startup-hook . recentf-mode)
  :config
  (setq recentf-save-file (concat user-emacs-cache-directory "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$" "COMMIT_EDITMSG\\"
              (concat "^" (file-truename user-emacs-local-directory)))))

;; Smart paranthesis
(use-package smartparens
  :hook (emacs-startup-hook . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autowrap-region t
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3))

;; Make smart paranthesis play nice with evil.
(use-package evil-smartparens
  :hook (emacs-startup-hook)
  :requires smartparens
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; Highlight indentations
(use-package highlight-indent-guides
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))

;; visualize for better undo
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (add-hook 'emacs-startup-hook #'global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat user-emacs-cache-directory "undo-tree-hist/")))
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; Highlight all symbols that match the selected.
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("C-h" . highlight-symbol))

(use-package evil
  :config
  (evil-mode))

(use-package evil-goggles
  :hook (emacs-startup-hook . evil-goggles-mode)
  :init
  (setq evil-goggles-duration 0.1
        evil-goggles-enable-delete nil))

;; Commenting blocks of code.
(use-package evil-nerd-commenter :defer t)

;;
;; Line numbers
;;
(defvar api-line-numbers-style 'relative
  "The style to use for the line number display that emulates
  `display-line-numbers' styles, which are:
t           Ordinary line numbers
'relative   Relative line numbers")

(defun api|enable-line-numbers ()
  (interactive)
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers api-line-numbers-style)
  (if (eq api-line-numbers-style 'relative)
        (linum-relative-mode)
      (linum-mode))))

(defun api|disable-line-numbers ()
  (interactive)
  (when (boundp 'display-line-numbers)
    (setq display-line-numbers nil))
  (linum-mode 0))

(use-package linum-relative
  :unless (boundp 'display-line-numbers)
  :diminish linum-relative-mode
  :commands linum-relative-mode
  :init
  (linum-mode 0)
  (setq linum-format "%4d"
        linum-relative-current-symbol ""))

(add-hook 'prog-mode-hook #'api|enable-line-numbers)
(add-hook 'text-mode-hook #'api|enable-line-numbers)
(add-hook 'org-mode-hook #'api|disable-line-numbers)

;; show fill column
(use-package fill-column-indicator
  :diminish (fci-mode . " ⓕ")
  :defer t
  :init
  (dolist (hooks '(prog-mode-hook
                   text-mode-hook
                   latex-mode-hook))
    (add-hook hooks (lambda () (fci-mode 1)))))

(provide 'init-editor)
;;; init-editor.el ends here
