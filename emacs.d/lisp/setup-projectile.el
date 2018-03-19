;;; setup-projectile.el --- Projectile -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package projectile
  :after helm
  :diminish (projectile-mode . " ⓟ")
  :ensure helm-projectile
  :hook (emacs-startup-hook . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
        projectile-enable-caching (not noninteractive)
        projectile-cache-file (concat user-emacs-cache-directory "projectile.cache")
        projectile-known-projects-file (concat user-emacs-cache-directory "projectile.projects")
        projectile-require-project-root nil
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  :config
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'helm)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
