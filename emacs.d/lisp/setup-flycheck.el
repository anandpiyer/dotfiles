;;; setup-flycheck.el --- Flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (global-flycheck-mode 1)
  :diminish
  (flycheck-mode))

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
