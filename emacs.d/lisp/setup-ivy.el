;;; setup-ivy.el --- Ivy setup
;;
;;; Commentary:
;;
;; Setup ivy and related packages.
;;
;;; Code:
(use-package ivy
  :commands ivy-switch-buffer
  :diminish ivy-mode
  :config
 (progn (ivy-mode)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")))

(use-package swiper
  :commands swiper)

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)
   ))

(provide 'setup-ivy)
;;; setup-ivy.el ends here
