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

(use-package tex
  :ensure nil
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t
          TeX-show-compilation t
          reftex-plug-into-AUCTeX t
          TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-source-correlate-start-server t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
  :config
  (progn
    (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
      (push '("PDF Tools" TeX-pdf-tools-sync-view) TeX-view-program-list))
    (add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdf")))
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))

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

(defun api/current-bib ()
  "Set the bibliography to the current directory."
  (interactive)
  (setq bibtex-completion-bibliography
        (concat (file-name-directory buffer-file-name) "references.bib"))
  (message (concat "Local bib file is " (file-name-directory
                                   buffer-file-name) "references.bib")))

(use-package ivy-bibtex
  :defer t
  :init
  (setq bibtex-completion-bibliography `(,(concat org-root-directory "references/references.bib"))
        bibtex-completion-library-path (concat org-root-directory "references/pdfs/")
        bibtex-completion-notes-path (concat org-root-directory "references/notes.org")))

(provide 'setup-tex)

;;; setup-tex.el ends here
