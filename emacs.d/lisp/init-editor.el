;;; init-editor.el --- Editor related settings
;;; Commentary:
;;; Code:

;; hybrid relative line number
(use-package nlinum
  :init
  (progn
    (setq nlinum-format "%4d")
    (global-nlinum-mode t)))

(use-package nlinum-relative
  :init
  (progn
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)))

(size-indication-mode t)
(global-hl-line-mode t)
(show-paren-mode)
(line-number-mode t)
(column-number-mode t)
(setq column-number-mode t)

(setq show-paren-delay 0)

; use spaces instead of tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

; use a single spaces at sentence ending
(setq sentence-end-double-space nil)

; delete selections with keypress
(delete-selection-mode t)

(use-package visual-line-mode
  :ensure nil
  :diminish (visual-line-mode . " Ⓛ")
  :init (global-visual-line-mode 1))

;; show fill column
(setq-default fill-column 80)
(use-package fill-column-indicator
  :diminish (fci-mode . " ⓕ")
  :defer t
  :init
  (dolist (hooks '(prog-mode-hook
                   text-mode-hook
                   latex-mode-hook))
    (add-hook hooks (lambda () (fci-mode 1)))))

;; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; aggressive indentation
(use-package aggressive-indent
  :diminish (aggressive-indent-mode . " Ⓘ")
  :defer t
  :config
  (progn (global-aggressive-indent-mode 1)
         (add-to-list 'aggressive-indent-excluded-modes
                      'text-mode)))

;; smarter paranthesis
(use-package smartparens
  :diminish (smartparens-mode)
  :defer t
  :config
  (progn (require 'smartparens-config)
         (smartparens-global-mode 1)))

(use-package evil-smartparens
  :diminish (evil-smartparens-mode)
  :defer t
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(provide 'init-editor)
;;; init-editor.el ends here
