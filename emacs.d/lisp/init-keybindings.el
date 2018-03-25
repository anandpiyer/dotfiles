;;; init-keybindings.el --- Key bindings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Initialize key bindings
;;
;;; Code:

(defvar my-leader-key "SPC"
  "Anand's leader prefix key.")

(use-package hydra
  :after evil
  :config
  (setq lv-use-seperator t)

  (defhydra api@text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))

  (defhydra api@window-nav (:hint nil)
    "
Move   : _h_: far left         _j_: very bottom      _k_:very top       _l_:far right      _s_: swap with other
Resize : _+_: increase height  _-_: decrease height  _<_:decrease width _>_:increase width _=_: balance
"
    ("h" evil-window-move-far-left)
    ("j" evil-window-move-very-bottom)
    ("k" evil-window-move-very-top)
    ("l" evil-window-move-far-right)
    ("s" ace-swap-window)

    ("+" evil-window-increase-height)
    ("-" evil-window-decrease-height)
    ("<" evil-window-decrease-width)
    (">" evil-window-increase-width)
    ("=" balance-windows)

    ("q" nil)))

(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-define-key :states '(normal visual emacs)
                      :keymaps 'global
                      :prefix my-leader-key

                      "SPC" 'helm-M-x
                      "r" 'evil-fill-and-move
                      
                      "b" '(:ignore t :which-key "buffer")
                      "bs" 'save-buffer
                      "bb" 'helm-mini
                      "bk" 'kill-this-buffer
                      "bn" 'evil-buffer-new
                      "bh" 'previous-buffer
                      "bl" 'next-buffer

                      "l" '(:ignore t :which-key "window layout")
                      "lp" 'eyebrowse-prev-window-config
                      "ln" 'eyebrowse-next-window-config
                      "lc" 'eyebrowse-close-window-config
                      "lr" 'eyebrowse-rename-window-config
                      "ld" 'eyebrowse--delete-window-config
                      "ll" 'eyebrowse-last-window-config
                      "l1" 'eyebrowse-switch-to-window-config-1
                      "l2" 'eyebrowse-switch-to-window-config-2
                      "l3" 'eyebrowse-switch-to-window-config-3

                      "f" '(:ignore t :which-key "file")
                      "ff" 'helm-find-files

                      "g" '(:ignore t :which-key "git")
                      "gs" 'magit-status
                      "gd" 'magit-diff
                      "gc" 'magit-commit
                      "gp" 'magit-push

                      "h" '(:ignore t :which-key "hydras")
                      "ht" '(api@text-zoom/body :which-key "Text zoom")
                      "hw" '(api@window-nav/body :which-key "Window navigation")
                      "hc" '(api@multiple-cursors/body :which-key "Multiple cursors")

                      "o" '(:ignore t :which-key "org-mode")
                      "oa" 'org-agenda
                      "oc" 'org-capture
                      "od" 'org-deadline
                      "or" 'org-refile
                      "os" 'org-schedule

                      "p" '(:ignore t :which-key "project")
                      "pf" 'projectile-find-file
                      "ps" 'projectile-switch-project
                      "pr" 'projectile-recentf
                      "px" 'projectile-invalidate-cache

                      "s" '(:ignore t :which-key "search")
                      "sb" '(swiper-helm :which-key "this buffer")
                      "sB" '(swiper-all :which-key "all open buffers")
                      "sf" '(helm-do-grep-ag :which-key "All files in this directory")

                      "t" '(:ignore t :which-key "toggle")
                      "tc" '(evilnc-comment-or-uncomment-lines :which-key "comments")
                      "ti" 'highlight-indent-guides-mode
                      "tt" 'flyspell-mode
                      "ts" 'flycheck-mode
                      "th" 'highlight-symbol

                      "v" #'er/expand-region
                      "V" #'er/contract-region

                      "w" '(evil-window-map :which-key "window")

                      "y" '(:ignore t :which-key "snippets")
                      "yn" 'yas-new-snippet
                      "yi" 'yas-insert-snippet
                      "yv" 'yas-visit-snippet-file

                      "TAB" 'ace-window))

(provide 'init-keybindings)

;;; init-keybindings.el ends here
