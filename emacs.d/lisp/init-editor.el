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
  (setq recent-save-file (concat user-emacs-cache-directory "recentf")
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

;; hybrid relative line number
(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (linum-mode -1)
  (setq linum-format "%4d"
        linum-relative-current-symbol ""
        linum-relative-backend 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  (add-hook 'text-mode-hook 'linum-relative-mode))
;;
;; Line numbers
;;

;; (defvar api-line-numbers-style 'relative
;;   "The style to use for the line number display.
;; Accepts the same arguments as `display-line-numbers', which are:
;; nil         No line numbers
;; t           Ordinary line numbers
;; 'relative   Relative line numbers")

;; (defun api|enable-line-numbers (&optional arg)
;;   "Enables the display of line numbers, using `display-line-numbers' (in Emacs
;; 26+) or `nlinum-mode'.
;; See `api-line-numbers-style' to control the style of line numbers to display."
;;   (cond ((boundp 'display-line-numbers)
;;          (setq display-line-numbers
;;                (pcase arg
;;                  (+1 api-line-numbers-style)
;;                  (-1 nil)
;;                  (_ api-line-numbers-style))))
;;         ((eq api-line-numbers-style 'relative)
;;          (if (= arg -1)
;;              (nlinum-relative-off)
;;            (nlinum-relative-on)))
;;         ((not (null api-line-numbers-style))
;;          (nlinum-mode (or arg +1)))))

;; (defun api|disable-line-numbers ()
;;   "Disable the display of line numbers."
;;   (api|enable-line-numbers -1))

;; (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
;;   (add-hook hook #'api|enable-line-numbers))

;; (use-package nlinum
;;   :unless (boundp 'display-line-numbers)
;;   :commands nlinum-mode
;;   :init
;;   (defvar api-line-number-lpad 4
;;     "How much padding to place before line numbers.")
;;   (defvar api-line-number-rpad 1
;;     "How much padding to place after line numbers.")
;;   (defvar api-line-number-pad-char 32
;;     "Character to use for padding line numbers.
;; By default, this is a space character. If you use `whitespace-mode' with
;; `space-mark', the whitespace in line numbers will be affected (this can look
;; ugly). In this case, you can change this to ?\u2002, which is a unicode
;; character that looks like a space that `whitespace-mode' won't affect.")
;;   :config
;;   (setq nlinum-highlight-current-line t)

;;   ;; Fix lingering hl-line overlays (caused by nlinum)
;;   (add-hook 'hl-line-mode-hook
;;     (remove-overlays (point-min) (point-max) 'face 'hl-line))

;;   (defun api-nlinum-format-fn (line _width)
;;     "A more customizable `nlinum-format-function'. See `doom-line-number-lpad',
;; `doom-line-number-rpad' and `doom-line-number-pad-char'. Allows a fix for
;; `whitespace-mode' space-marks appearing inside the line number."
;;     (let ((str (number-to-string line)))
;;       (setq str (concat (make-string (max 0 (- api-line-number-lpad (length str)))
;;                                      api-line-number-pad-char)
;;                         str
;;                         (make-string api-line-number-rpad api-line-number-pad-char)))
;;       (put-text-property 0 (length str) 'face
;;                          (if (and nlinum-highlight-current-line
;;                                   (= line nlinum--current-line))
;;                              'nlinum-current-line
;;                            'linum)
;;                          str)
;;       str))
;;   (setq nlinum-format-function #'api-nlinum-format-fn)

;;   (defun api|init-nlinum-width ()
;;     "Calculate line number column width beforehand (optimization)."
;;     (setq nlinum--width
;;           (length (save-excursion (goto-char (point-max))
;;                                   (format-mode-line "%l")))))
;;   (add-hook 'nlinum-mode-hook #'api|init-nlinum-width))

;; ;Fixes disappearing line numbers in nlinum and other quirks
;; (use-package nlinum-hl
;;   :unless (boundp 'display-line-numbers)
;;   :after nlinum
;;   :config
;;   ;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
;;   ;; line numbers tend to vanish next to code blocks.
;;   (advice-add #'markdown-fontify-code-block-natively
;;               :after #'nlinum-hl-do-markdown-fontify-region)
;;   ;; When using `web-mode's code-folding an entire range of line numbers will
;;   ;; vanish in the affected area.
;;   (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)
;;   ;; Changing fonts can leave nlinum line numbers in their original size; this
;;   ;; forces them to resize.
;;   (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows))

;; (use-package nlinum-relative
;;   :unless (boundp 'display-line-numbers)
;;   :commands nlinum-relative-mode
;;   :after evil
;;   :config (nlinum-relative-setup-evil))

;;(use-package visual-line-mode
;;   :ensure nil
;;   :diminish visual-line-mode
;;   :config (global-visual-line-mode 1))

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
