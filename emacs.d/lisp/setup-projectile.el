;;; setup-projectile.el --- Projectile related things
;;; Commentary:
;;; Code:
(use-package projectile
  :diminish (projectile-mode . " ⓟ")
  :defer t
  :commands
  (projectile-ag
   projectile-switch-to-buffer
   projectile-invalidate-cache
   projectile-find-dir
   projectile-find-file
   projectile-find-file-dwim
   projectile-find-file-in-directory
   projectile-ibuffer
   projectile-kill-buffers
   projectile-multi-occur
   projectile-multi-occur
   projectile-switch-project
   projectile-recentf
   projectile-remove-known-project
   projectile-cleanup-known-projects
   projectile-cache-current-file
   projectile-project-root
   )
  :config
  (progn
    (projectile-global-mode 1)
    (use-package counsel-projectile)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
