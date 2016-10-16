;;; setup-evil.el --- Evil related settings
;;; Commentary:
;;; Code:
(use-package evil
  :defer t
  :init
  (evil-mode 1)

  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "w" 'save-buffer
      "e" 'find-file
      "bs" 'switch-to-buffer
      "bd" 'kill-this-buffer
      "q" 'quit-window
      "fr" 'evil-fill-and-move)
    (evil-leader/set-key-for-mode 'latex-mode
      "fr" 'LaTeX-fill-region
      "fp" 'LaTeX-fill-paragraph)))
    
(provide 'setup-evil)
;;; setup-evil.el ends here
