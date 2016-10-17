;;; init-editor.el --- Editor related settings
;;; Commentary:
;;; Code:

;; hybrid relative line number
(use-package nlinum
  :init
  (setq nlinum-format "%4d")
  (global-nlinum-mode t)

  (use-package nlinum-relative
    :init
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

(global-visual-line-mode 1)

;; show fill column
(setq-default fill-column 80)
(use-package fill-column-indicator
  :defer t
  :init
  (dolist (hooks '(prog-mode-hook
                   text-mode-hook
                   latex-mode-hook))
    (add-hook hooks (lambda () (fci-mode 1)))))

;; turn on auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

(provide 'init-editor)
;;; init-editor.el ends here
