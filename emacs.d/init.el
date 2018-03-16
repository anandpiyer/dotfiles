;;; init.el --- Personal emacs configuration of Anand Iyer

;;; Commentary:
;; https://github.com/wasamasa/dotemacs/blob/master/init.org
;; https://github.com/bling/dotemacs
;; https://github.com/tonini/emacs.d

;;; Code:

;; The following comment avoids emacs automatically adding (package-initialize)
;; (package-initialize)

; increase gc-cons-threshold during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar api--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

; always prefer newer byte code
(setq load-prefer-newer t)

(defvar active-modules-file
   (expand-file-name "active-modules.el" user-emacs-directory)
   "This file contains a list of modules that will be loaded.")

(defvar org-root-directory
;;  "~/OneDrive/org-mode/"
  "~/Dropbox/org-mode/"
  "Directory where org files are stored.")

(defconst user-emacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
   "Directory where cache files are stored.")

(defconst user-emacs-temp-directory
  (expand-file-name (concat user-emacs-directory "tmp/"))
   "Directory where temp files are stored.")

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

(add-hook 'emacs-startup-hook
           (setq file-name-handler-alist api--file-name-handler-alist))

(add-hook 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(provide 'init)

;;; init.el ends here
