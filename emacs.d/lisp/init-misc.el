;;; init-misc.el --- Miscellaneous stuff
;;; Commentary:
;;; Code:

; auto completion with company
(use-package company
  :init
  (global-company-mode)
  :diminish
  (company-mode))

; syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode 1)
  :diminish
  (flycheck-mode))
  
(provide 'init-misc)
;;; init-misc.el ends here
