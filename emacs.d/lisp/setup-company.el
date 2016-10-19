;;; setup-company.el --- Company related setup
;;; Commentary:
;;; Code:

(use-package company
  :defer t
  :config
  (global-company-mode)
  :diminish
  (company-mode . " ⓐ"))

(provide 'setup-company)
;;; setup-company.el ends here
