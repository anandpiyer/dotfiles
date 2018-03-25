;;; setup-ivy.el --- Ivy setup -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Setup ivy and related packages.
;;
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :init
  (add-hook 'emacs-startup-hook #'ivy-mode)
  :config
  (setq ivy-height 12
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil)

  (nconc ivy-sort-functions-alist
         '((persp-kill-buffer   . nil)
           (persp-remove-buffer . nil)
           (persp-add-buffer    . nil)
           (persp-switch        . nil)
           (persp-window-switch . nil)
           (persp-frame-switch  . nil)
           (+workspace/switch-to . nil)
           (+workspace/delete . nil))))


(use-package swiper :commands (swiper swiper-all))

(use-package counsel :requires ivy)

;; Used by `counsel-M-x'
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat user-emacs-cache-directory "/smex-items"))
(smex-initialize))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
