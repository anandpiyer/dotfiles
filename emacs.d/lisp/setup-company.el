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
          company-idle-delay 0.2
          company-require-match nil
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
  (progn
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

    (global-company-mode 1)))

(provide 'setup-company)
;;; setup-company.el ends here
