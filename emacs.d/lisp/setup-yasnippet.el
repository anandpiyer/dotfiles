;;; setup-yasnippet.el --- yanippet related setup
;;; Commentary:
;;; Code:
(use-package yasnippet
  :defer t
  :commands (yas-global-mode yas-minor-mode)
  :init
  (progn
    (defvar yas-global-mode nil)
    (add-hook 'yas-minor-mode '(prog-mode-hook
                                markdown-mode-hook
                                latex-mode-hook
                                org-mode-hook))))

(provide 'setup-yasnippet)
;;; setup-yasnippet ends here
