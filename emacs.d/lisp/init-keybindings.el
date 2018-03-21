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
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu
"
    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" switch-to-buffer)
    ("f" find-file)

    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

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

                      "c" '(:ignore t :which-key "window configuration")
                      "ch" 'eyebrowse-prev-window-config
                      "cp" 'eyebrowse-prev-window-config
                      "cl" 'eyebrowse-next-window-config
                      "cn" 'eyebrowse-next-window-config
                      "cc" 'eyebrowse-close-window-config
                      "cr" 'eyebrowse-rename-window-config
                      "cd" 'eyebrowse--delete-window-config

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
                      "hc" '(api/multiple-cursors-hydra/body :which-key "Multiple cursors")


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
                      "ti" 'highlight-indentation-mode
                      "tt" 'flyspell-mode
                      "ts" 'flycheck-mode
                      "th" 'highlight-symbol

                      "w" '(evil-window-map :which-key "window")

                      "y" '(:ignore t :which-key "snippets")
                      "yn" 'yas-new-snippet
                      "yi" 'yas-insert-snippet
                      "yv" 'yas-visit-snippet-file

                      "TAB" 'ace-window))

(provide 'init-keybindings)

;;; init-keybindings.el ends here
