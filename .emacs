;; Common lisp facilities
(require 'cl)

;; TODO: Probably should change if porting on Windows
(defun get-home-directory ()
  (cond
   ((getenv "emacsHOME"))
   ((getenv "HOME"))
   )
)

;; --------------------- load paths ---------------------
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp"))
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/customization"))
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/customization/langs"))

;; color themes
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/color-theme-6.6.0/"))

;; org mode
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/org-7.7/lisp"))
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/org-7.7/contrib/lisp"))

;; CEDET 1.1
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/cedet-1.1/common"))

;; ECB
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/ecb-2.40"))

;; Js2-mode
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/js2-mode"))

;; elib
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/elib-1.0"))

;; workgroups
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/workgroups"))
;; ------------------------------------------------------

;; ---------------- Org mode preloader ----------------
(load "org-mode-preload")

;; ---------------- Langs -----------------------------
(load "cpp")
(load "graphviz-dot-mode")
(load "perl")
(load "js2-mode-custom")

;; --------------- gnus message reader ----------------
(load "gnus")

;; ------------------- Start the loading ----------------
(load "general")
(load "utility")
(load "cedet-preload")
(load "browser")
(load "color-theme-custom")
(load "workgroups-custom")

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(inhibit-startup-screen t)
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(preview-scale-function 1.1)
 '(safe-local-variable-values nil)
 '(save-abbrevs nil))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "#FFFF80"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#BBAAFF")))))

