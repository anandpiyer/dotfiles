;;; setup-misc.el --- IDO related setting
;;; Commentary:
;;; Code:

(use-package ido
  :config
  (setq ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-use-virtual-buffers t
        ido-enable-dot-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode t)
  (ido-everywhere t)

  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode t))

  (use-package flx-ido
    :config
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights
    (setq ido-use-faces nil))

  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-show-count t)))
  
  
(provide 'setup-ido)
;;; setup-ido.el ends here
