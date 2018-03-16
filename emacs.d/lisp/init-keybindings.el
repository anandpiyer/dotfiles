;;; init-keybindings.el --- Key bindings
;;
;;; Commentary:
;;
;; Initialize key bindings
;;
;;; Code:

(use-package general
  :init
  (setq my-leader "SPC"
        general-default-states '(normal visual emacs))
  
  :config
  (general-evil-setup t)

  (general-define-key :prefix my-leader
                      "SPC" 'helm-M-x
                      "w" 'save-buffer
                      "s" '(:ignore t :which-key "Search")
                      "sb" '(swiper-helm :which-key "Current buffer")
                      "sB" '(swiper-all :which-key "All open buffers")
                      "sf" '(helm-do-grep-ag :which-key "All files in this directory")
                      "e" 'helm-find-files
                      "b" '(:ignore t :which-key "Buffer")
                      "bs" 'helm-mini
                      "bd" 'kill-this-buffer
                      "q" 'delete-window
                      "f" '(:ignore t :which-key "Frame")
                      "fd" 'delete-frame
                      "c" '(:ignore t :which-key "Commenter")
                      "ct" '(evilnc-comment-or-uncomment-lines :which-key "Toggle comments")
                      "g" '(:ignore t :which-key "Git")
                      "gs" 'magit-status
                      "gd" 'magit-diff
                      "gc" 'magit-commit
                      "gp" 'magit-push
                      "o" '(:ignore t :which-key "Org-Mode")
                      "oa" 'org-agenda
                      "oc" 'org-capture
                      "od" 'org-deadline
                      "or" 'org-refile
                      "os" 'org-schedule
                      "h" '(:ignore t :which-key "Hydras")
                      "ht" '(api/themes-hydra/body :which-key "Switch theme")
                      "hc" '(api/multiple-cursors-hydra/body :which-key "Multiple cursors"))

  (general-define-key :prefix my-leader
                      :keymaps 'LaTeX-mode-map
                      "f" '(:ignore t :which-key "Format")
                      "fr" 'LaTeX-fill-region
                      "fp" 'LaTeX-fill-paragraph)
  
  ;; (general-define-key :prefix my-leader
  ;;                     :keymaps 'org-mode-map
  ;;                     "o" '(:ignore t :which-key "Org-mode")
  ;;                     "oc" 'org-capture
  ;;                    "or" 'org-refile))
)
(provide 'init-keybindings)

;;; init-keybindings.el ends here
