;;; init-tex.el --- latex configuration
;;; commentary:
;;; code:
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/Library/TeX/texbin"
         ":/usr/local/bin"))
(setq exec-path
      (append exec-path
              '("/Library/TeX/texbin"
                "/usr/local/bin")))

(use-package auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          TeX-show-compilation t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdf")))

(use-package auctex-latexmk
    :defer t
    :init
    (progn
      (setq auctex-latexmk-inherit-TeX-PDF-mode t))
    :config
    (auctex-latexmk-setup))

(provide 'init-tex)

;;; init-tex.el ends here
