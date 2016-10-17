;;; setup-helm.el --- helm setup
;;
;;; Commentary:
;;
;; setting related to Helm.
;;
;;; Code:

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (helm-mode))
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :config
  (progn
    (setq helm-split-window-in-side-p t
        helm-ff-skip-boring-files t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
    (setq helm-autoresize-min-height 10
          helm-autoresize-max-height 30)
    (helm-autoresize-mode 1)))

(use-package helm-swoop
  :defer t
  :commands (helm-swoop)
  :bind (("C-c /" . helm-swoop)))

(provide 'setup-helm)
;;; setup-helm.el ends here
