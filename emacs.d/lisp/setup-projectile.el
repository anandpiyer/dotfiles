;;; setup-projectile.el --- Projectile related things
;;; Commentary:
;;; Code:
(use-package projectile
  :diminish (projectile-mode . " ⓟ")
  :defer t
  :config
  (progn
    (projectile-global-mode +1)
    (use-package counsel-projectile)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
