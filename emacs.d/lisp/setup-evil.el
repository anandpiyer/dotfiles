;;; setup-evil.el --- Evil related settings
;;; Commentary:
;;; Code:
(use-package evil
  :defer t
  :config
  ;; (setq evil-emacs-state-cursor '("red" box)
  ;;       evil-motion-state-cursor '("orange" box)
  ;;       evil-normal-state-cursor '("green" box)
  ;;       evil-visual-state-cursor '("orange" box)
  ;;       evil-insert-state-cursor '("red" bar)
  ;;       evil-replace-state-cursor '("red" bar)
  ;;       evil-operator-state-cursor '("red" hollow))
  (evil-mode 1))

(provide 'setup-evil)
;;; setup-evil.el ends here
