;;; setup-projectile.el --- Projectile -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package projectile
  :diminish (projectile-mode . " ⓟ")
  :ensure helm-projectile
  :defer t
  :init
  (setq projectile-sort-order 'recentf
        projectile-cache-file
        (concat user-emacs-cache-directory "projectile.cache")
        projectile-known-projects-file
        (concat user-emacs-cache-directory "projectile-bookmarks.eld"))
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'helm)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (helm-projectile-on)
    (projectile-mode)))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
