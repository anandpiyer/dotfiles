;;; setup-git.el --- git related packages
;;; Commentary:
;;; Code:
(use-package magit
  :defer t)

;; git-gutter and git-gutter-fringe don't play nice with nlinum.
;; (use-package diff-hl
;;   :defer t
;;   :config
;;   (global-diff-hl-mode t))

(use-package git-gutter
  :diminish git-gutter-mode
  :defer t
  :config
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (setq git-gutter:update-interval 2
          git-gutter:modified-sign "▪"
          git-gutter:added-sign "+"
          git-gutter:deleted-sign "-"
          git-gutter:hide-gutter t
          git-gutter:verbosity 0)))

(provide 'setup-git)
;;; setup-git.el ends here
