;;; setup-evil.el --- Evil related settings
;;; Commentary:
;;; Code:
(use-package evil
  :config
  (evil-mode 1)

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "w" 'save-buffer
      "e" 'find-file
      "b" 'switch-to-buffer
      "d" 'kill-this-buffer
      "q" 'quit-window
      "r" 'evil-fill-and-move)))
    
(provide 'setup-evil)
;;; setup-evil.el ends here
