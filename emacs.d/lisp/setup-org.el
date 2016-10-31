;;; setup-org.el --- org setup
;;; Commentary:
;;
;; Some settings related to org mode.  Majorly based on:
;; https://m.reddit.com/r/emacs/comments/4gudyw/help_me_with_my_orgmode_workflow_for_notetaking/
;; https://github.com/sriramkswamy/dotemacs/
;; 
;;; Code:
(use-package org-plus-contrib
  :defer t
  :bind ("C-c c" . org-capture)
  :init
  (setq org-directory "~/org-mode/"
        org-default-notes-file (concat org-directory "organizer.org")
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-completion-use-ido t
        org-agenda-window-setup 'current-window
        org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  (setq org-agenda-files
        (list (concat org-directory "organizer.org")
              (concat org-directory "references/notes.org"))
        org-deadline-warning-days 7
        org-agenda-span 'fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t)
  
  (setq org-capture-templates
        (quote (
                ;; For notes on paper readings
                ("p"
                 "Paper"
                 entry
                 (file+headline (concat org-directory "references/notes.org") "Papers")
                 "* %^{Title} %(org-set-tags)  :paper: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nNotes:\n%?"
                 :prepend t
                 :empty-lines 1
                 :created t
                 :kill-buffer t)

                ;; For taking notes on random things
                ("n"
                 "Note"
                 entry
                 (file+headline (concat org-directory "organizer.org") "Notes")
                 "* %? %(org-set-tags)  :note:\n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i"
                 :prepend t
                 :empty-lines 1
                 :created t
                 :kill-buffer t)

                ;; To-dos
                ("t"
                 "Tasks"
                 entry
                 (file+headline (concat org-directory "organizer.org") "Inbox")
                 "* TODO %^{Todo} %(org-set-tags)  :task:\n:PROPERTIES:\n:Created: %U\n:END:\n%i\n%?"
                 :prepend t
                 :empty-lines 1
                 :created t
                 :kill-buffer t)))))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package evil-org
  :commands (evil-org-mode evil-org-recompute-clocks)
  :init (add-hook 'org-mode-hook 'evil-org-mode))

;; superior pdf tools compared to docview
(use-package pdf-tools
  :defer t
  :init (pdf-tools-install))

;; ivy-bibtex is used by org-ref, so set it up first.
(use-package ivy-bibtex
  :defer t
  :init
  (setq bibtex-completion-bibliography `(,(concat org-directory "references/references.bib"))
        bibtex-completion-library-path (concat org-directory "references/pdfs/")
        bibtex-completion-notes-path (concat org-directory "references/notes.org")))

;; awesome mode for citations and stuff!
(use-package org-ref
  :defer t
  :ensure ivy-bibtex
  :init
  (progn (setq org-ref-completion-library 'org-ref-ivy-cite
               org-ref-notes-directory (concat org-directory "references/notes")
               org-ref-bibliography-notes (concat org-directory "references/notes.org")
               org-ref-default-bibliography `(,(concat org-directory "references/references.bib"))
               org-ref-pdf-directory (concat org-directory "references/pdfs/"))
         (add-hook 'org-mode-hook (lambda ()
                                    (require 'org-ref)
                                    (require 'org-ref-latex)
                                    (require 'org-ref-pdf)
                                    (require 'org-ref-url-utils)))))

;; interleave PDFs within notes.
(use-package interleave
  :defer t
  :commands (interleave interleave-pdf-mode))

(provide 'setup-org)
;;; setup-org.el ends here
