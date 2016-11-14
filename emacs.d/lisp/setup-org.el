;;; setup-org.el --- org setup
;;; Commentary:
;;
;; Some settings related to org mode.  Majorly based on:
;; https://m.reddit.com/r/emacs/comments/4gudyw/help_me_with_my_orgmode_workflow_for_notetaking/
;; https://github.com/sriramkswamy/dotemacs/
;; 
;;; Code:

;; http://stackoverflow.com/questions/21073859/is-there-a-way-with-org-capture-templates-to-not-insert-a-line-if-initial-conten
(defun v-i-or-nothing ()
  "Use initial content only if available."
  (let ((v-i (plist-get org-store-link-plist :initial)))
    (if (equal v-i "")
        ""
      (concat v-i "\n"))))

(use-package org
  :defer t
  :bind ("C-c c" . org-capture)
  :init
  (setq org-directory org-root-directory
        org-default-notes-file (concat org-directory "organizer.org")
        org-startup-with-inline-images t
        org-src-fontify-natively t
        org-imenu-depth 8
        org-log-into-drawer t
        org-agenda-window-setup 'current-window
        org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  (setq org-agenda-files
        (list (concat org-directory "organizer.org")
              (concat org-directory "references/notes.org"))
        org-deadline-warning-days 7
        org-agenda-span 'fortnight
        org-agenda-skip-scheduled-if-deadline-is-shown t)

  (setq org-capture-templates
        (quote (("t" "Todo" entry
                 (file+headline org-default-notes-file "Inbox")
                 "* TODO %^{Todo}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%(v-i-or-nothing)"
                 :empty-lines 1
                 :immediate-finish t)))))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package evil-org
  :diminish (evil-org-mode . "ⓔ")
  :commands (evil-org-mode evil-org-recompute-clocks)
  :init (add-hook 'org-mode-hook 'evil-org-mode))

;; use brew to install pdf-tools so that epdfinfo gets installed properly:
;;     brew install homebrew/emacs/pdf-tools
;; and set the path to epdfinfo from brew installation.
 (use-package pdf-tools
   :defer t
   :mode (("\\.pdf\\'" . pdf-view-mode))
   :config
   (progn
     (custom-set-variables '(pdf-tools-handle-upgrades nil))
     (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
     (pdf-tools-install)))

(use-package org-ref
  :defer t
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

;; interleave PDFs with notes. This needs to be after pdf-tools. Also, interleave
;; needs to be removed and reinstalled everytime pdf-tools is updated.
;; See: https://github.com/rudolfochrist/interleave/issues/31#issuecomment-252351991
(use-package interleave
  :defer t
  :init
  (progn
    (setq interleave-org-notes-dir-list `(,(concat org-directory "references")))
    (with-eval-after-load 'doc-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf doc-view-mode-map))
    (with-eval-after-load 'pdf-view
      (bind-key "i" #'interleave--open-notes-file-for-pdf pdf-view-mode-map))))

;; notational velocity and nvALT replacement.
(use-package deft
  :commands (deft)
  :init
  (progn
    (setq deft-directory "~/OneDrive/Notes"
          deft-extensions '("org" "md" "txt" "markdown")
          deft-text-mode 'org-mode
          deft-use-filename-as-title t
          deft-use-filter-string-for-filename t)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'setup-org)
;;; setup-org.el ends here
