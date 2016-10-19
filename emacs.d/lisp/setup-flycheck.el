;;; setup-flycheck.el --- Flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer t
  :diminish (flycheck-mode . " ⓢ")
  :config (global-flycheck-mode 1))

(provide 'setup-flycheck)

;;; setup-flycheck.el ends here
