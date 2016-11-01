;;; setup-tex.el --- latex configuration
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

(use-package bibtex
  :ensure nil
  :init
  (setq bibtex-align-at-equal-sign t
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
        bibtex-autokey-titleword-length 0
        bibtex-autokey-titlewords 0
        bibtex-autokey-year-length 4))

(provide 'setup-tex)

;;; setup-tex.el ends here
