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
                      "SPC" 'counsel-M-x
                      "e" 'find-file
                      "w" 'save-buffer
                      "b" '(:ignore t :which-key "Buffer")
                      "bs" 'switch-to-buffer
                      "bd" 'kill-this-buffer
                      "q" 'quit-window
                      "fr" 'evil-fill-and-move
                      "c" '(:ignore t :which-key "Commenter")
                      "ci" 'evilnc-comment-or-uncomment-lines
                      "g" '(:ignore t :which-key "Git")
                      "gs" 'magit-status
                      "gd" 'magit-diff
                      "o" '(:ignore t :which-key "Org-Mode")
                      "oa" 'org-agenda
                      "oc" 'org-capture
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
