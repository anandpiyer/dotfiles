;;; setup-org.el --- org setup
;;; Commentary:
;;
;; Some settings related to org mode.
;;
;;; Code:
(use-package org
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
  :config
  (setq org-capture-templates
        (quote (("T" "Tasks" entry
                 (file+headline (concat org-directory "organizer.org") "Inbox")
                 "* TODO %^{Task}\nSCHEDULED: %t\n"
                 :immediate-finish t)
                ("j" "Journal" entry
                 (file+datetree (concat org-directory "journal.org"))
                 "* %?\nEntered on %U\n  %i\n  %a")
                ("J" "Journal entry with date" plain
                 (file+datetree+prompt (concat org-directory "journal.org"))
                 "%K - %a\n%i\n%?\n"
                 :unnarrowed t)
                ("n" "Notes" entry
                 (file+datetree (concat org-directory "organizer.org"))
                 "* %? :NOTE:\n%i\n%U\n")))))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package evil-org
  :commands (evil-org-mode evil-org-recompute-clocks)
  :init (add-hook 'org-mode-hook 'evil-org-mode))

(provide 'setup-org)
;;; setup-org.el ends here
