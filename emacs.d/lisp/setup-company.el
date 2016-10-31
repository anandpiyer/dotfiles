;;; setup-company.el --- Company related setup
;;; Commentary:
;;; Code:

(use-package company
  :diminish (company-mode . " ⓐ")
  :defer t
  :commands company-mode
  :init
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 4)
  :config
  (global-company-mode))

(provide 'setup-company)
;;; setup-company.el ends here
