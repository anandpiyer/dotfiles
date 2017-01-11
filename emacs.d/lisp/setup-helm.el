;;; setup-helm.el --- helm setup
;;
;;; Commentary:
;;
;; setting related to Helm.
;;
;;; Code:

(use-package helm
  :diminish helm-mode
  :defer t
  :commands (helm-M-x
             helm-find-files
             helm-buffers-list)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("TAB" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))

  :init
  (progn
    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t)

    ;; fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-locate-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)

    ;; hide minibuffer and use header line
    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

  :config
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (setq helm-ff-skip-boring-files t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-scroll-amount 8
          helm-ff-file-name-history-use-recentf t)
    (setq helm-autoresize-min-height 20
          helm-autoresize-max-height 0)
    (helm-autoresize-mode 1)))

(use-package helm-swoop
  :defer t
  :commands (helm-swoop)
  :bind (("C-c /" . helm-swoop)))

(provide 'setup-helm)
;;; setup-helm.el ends here
