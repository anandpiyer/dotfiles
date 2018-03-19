;;; setup-flycheck.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish (flycheck-mode . " ⓢ")
  :after evil
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  
  (defun +syntax-checkers|flycheck-buffer ()
  (when flycheck-mode
    (ignore-errors (flycheck-buffer))
    nil))
  (add-hook '+evil-esc-hook #'+syntax-checkers|flycheck-buffer t))

(provide 'setup-flycheck)

;;; setup-flycheck.el ends here
