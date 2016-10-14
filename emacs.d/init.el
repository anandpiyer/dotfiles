;;; init.el --- Personal emacs configuration of Anand Iyer

;;; Commentary:
;;; https://github.com/wasamasa/dotemacs/blob/master/init.org
;;; https://github.com/bling/dotemacs
;;; https://github.com/tonini/emacs.d

;;; Code:

;; Always prefer newer byte code
(setq load-prefer-newer t)

;; Boostrap use-package
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar active-modules-file (expand-file-name "active-modules.el" user-emacs-directory)
  "This file contains a list of modules that will be loaded.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-core)
(require 'init-ui)
(require 'init-editor)

(if (file-exists-p active-modules-file)
    (load active-modules-file))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
