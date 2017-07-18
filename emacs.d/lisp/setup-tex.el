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
          ;;TeX-show-compilation t
          reftex-plug-into-AUCTeX t
          TeX-view-program-selection '((output-pdf "PDF Viewer"))
          TeX-source-correlate-start-server t)
    (defadvice TeX-LaTeX-sentinel
        (around mg-TeX-LaTeX-sentinel-open-output activate)
      "Open output when there are errors."
      ;; Run `TeX-LaTeX-sentinel' as usual.
      ad-do-it
      ;; Check for the presence of errors.
      (when
          (with-current-buffer TeX-command-buffer
            (plist-get TeX-error-report-switches (intern (TeX-master-file))))
        ;; If there are errors, open the output buffer.
        (TeX-recenter-output-buffer nil)))
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
  :config
  (progn
    (setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
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

 (use-package company-auctex
    :defer t
    :config
    (progn
      (push 'company-auctex-labels company-backends-LaTeX-mode)
      (push 'company-auctex-bibs company-backends-LaTeX-mode)
      (push '(company-auctex-macros
              company-auctex-symbols
              company-auctex-environments)
            company-backends-LaTeX-mode)))

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

(use-package helm-bibtex
  :defer t
  :init
  (setq bibtex-completion-bibliography `(,(concat org-root-directory "papers/references.bib"))
        bibtex-completion-library-path (concat org-root-directory "papers/pdfs")
        bibtex-completion-notes-path (concat org-root-directory "papers/notes.org")))

(put 'TeX-command-extra-options 'safe-local-variable #'stringp)

(provide 'setup-tex)

;;; setup-tex.el ends here
