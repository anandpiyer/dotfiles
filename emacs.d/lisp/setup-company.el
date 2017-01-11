;;; setup-company.el --- Company related setup
;;; Commentary:
;;; Code:

(use-package company
  :diminish (company-mode . " ⓐ")
  :defer t
  :commands company-mode
  :init
  (progn
    (setq company-dabbrev-ignore-case nil
          company-dabbrev-code-ignore-case nil
          company-dabbrev-downcase nil
          company-idle-delay 0.1
          company-selection-wrap-around t
          company-tooltip-align-annotations 't
          company-minimum-prefix-length 2)

    ;; fci-mode conflicts with company-mode, see:
    ;; https://github.com/company-mode/company-mode/issues/180
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (global-company-mode))

(provide 'setup-company)
;;; setup-company.el ends here
