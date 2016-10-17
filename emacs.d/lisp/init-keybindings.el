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
                      "SPC" 'execute-extended-command
                      "e" 'find-file
                      "w" 'save-buffer
                      "b" '(:ignore t :which-key "Buffer")
                      "bs" 'switch-to-buffer
                      "bd" 'kill-this-buffer
                      "q" 'quit-window
                      "fr" 'evil-fill-and-move)

  (general-define-key :prefix my-leader
                      :keymaps 'LaTeX-mode-map
                      "f" '(:ignore t :which-key "Reformat")
                      "fr" 'LaTeX-fill-region
                      "fp" 'LaTeX-fill-paragraph))
  
(provide 'init-keybindings)

;;; init-keybindings.el ends here
