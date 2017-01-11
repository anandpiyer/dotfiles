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
                      "e" 'helm-find-files
                      "w" 'save-buffer
                      "b" '(:ignore t :which-key "Buffer")
                      "bs" 'helm-mini
                      "bd" 'kill-this-buffer
                      "q" 'delete-window
                      "fr" 'evil-fill-and-move
                      "c" '(:ignore t :which-key "Commenter")
                      "ci" 'evilnc-comment-or-uncomment-lines
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
                      "os" 'org-schedule)

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
