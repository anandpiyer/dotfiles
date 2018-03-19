;;; setup-git.el --- git related packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :after evil
  :commands (magit-status magit-blame)
  :config
  (setq magit-auto-revert-mode nil)
  ;; Switch to emacs state only while in `magit-blame-mode', then back when
  ;; its done (since it's a minor-mode).
  (add-hook 'magit-blame-mode-hook
            (evil-local-mode (if magit-blame-mode -1 +1))))

(use-package git-gutter
   :diminish git-gutter-mode
   :config
   (setq git-gutter:update-interval 2
         git-gutter:modified-sign " "
         git-gutter:added-sign " "
         git-gutter:deleted-sign " "
         git-gutter:hide-gutter t
         git-gutter:verbosity 0))

(use-package git-gutter-fringe
  :commands git-gutter-mode
  :after evil
  :init
  (setq-default fringes-outside-margins t)

  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'+version-control|git-gutter-maybe))
  :config
  
  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (defun +version-control|update-git-gutter ()
    "Refresh git-gutter on ESC. Return nil to prevent shadowing other `+evil-esc-hook' hooks."
    (when git-gutter-mode
      (git-gutter)
      nil))
  (add-hook '+evil-esc-hook #'+version-control|update-git-gutter t)

  (defhydra +hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                          :hint nil)
    "
                                     ╭─────────────────┐
  Movement   Hunk Actions     Misc.  │ gg: +%-4s(car (git-gutter:statistic))/ -%-3s(cdr (git-gutter:statistic)) │
  ╭──────────────────────────────────┴─────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup          ╭──────────────────────
     ^_j_^                          │[_q_] quit
     ^_G_^                          │[_Q_] Quit and disable"
      ("j" (progn (git-gutter:next-hunk 1)
                  (recenter)))
      ("k" (progn (git-gutter:previous-hunk 1)
                  (recenter)))
      ("g" (progn (goto-char (point-min))
                  (git-gutter:next-hunk 1)))
      ("G" (progn (goto-char (point-min))
                  (git-gutter:previous-hunk 1)))
      ("s" git-gutter:stage-hunk)
      ("r" git-gutter:revert-hunk)
      ("m" git-gutter:mark-hunk)
      ("p" git-gutter:popup-hunk)
      ("R" git-gutter:set-start-revision)
      ("q" nil :color blue)
      ("Q" (git-gutter-mode -1) :color blue)))
 
(provide 'setup-git)
;;; setup-git.el ends here
