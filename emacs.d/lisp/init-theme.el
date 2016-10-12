;;; init-theme.el --- theme
;;; commentary:
;;; code:

;; Set default font
(set-face-attribute 'default nil
                    :family "Input"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;(use-package gruvbox-theme
;  :config
;  (load-theme 'gruvbox t))

(use-package leuven-theme
  :config
  (load-theme 'leuven t))
  
;; powerline
(use-package powerline-evil)
(use-package powerline
  :init
  (powerline-evil-vim-color-theme))
(add-hook 'after-init-hook 'powerline-reset)

(provide 'init-theme)
;;; init-theme.el ends here
