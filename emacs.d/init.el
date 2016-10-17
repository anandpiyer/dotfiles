;;; init.el --- Personal emacs configuration of Anand Iyer

;;; Commentary:
;; https://github.com/wasamasa/dotemacs/blob/master/init.org
;; https://github.com/bling/dotemacs
;; https://github.com/tonini/emacs.d

;;; Code:

;; The following comment avoids emacs automatically adding (package-initialize)
;; (package-initialize)

; increase gc-cons-threshold during init
(setq gc-cons-threshold 100000000)

; always prefer newer byte code
(setq load-prefer-newer t)

(defvar active-modules-file
   (expand-file-name "active-modules.el" user-emacs-directory)
  "This file contains a list of modules that will be loaded.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-core)
(require 'init-ui)
(require 'init-editor)
(require 'init-keybindings)

(load active-modules-file)

; dont litter my init file!
(setq custom-file (concat user-emacs-directory "custom.el"))

(require 'server)
(unless (server-running-p) (server-start))

(provide 'init)

;;; init.el ends here
