(defun get-home-directory ()
  (getenv "HOME")
)

(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode nil)
(global-font-lock-mode t)
(column-number-mode 1)
(show-paren-mode 1)
;; (menu-bar-mode 0)
(tool-bar-mode 0)

(defun save-buffer-n-revert (&optional args)
  "When you save a buffer under a different extension, this will reload the correct modes for it."
  (interactive)
  (let ((orig-name (buffer-name)))
    (save-buffer args)
    (unless (string= orig-name (buffer-name))
      (revert-buffer nil t)
    )
  )
)
(global-set-key (kbd "C-x C-s") 'save-buffer-n-revert)

;; --------------------- load paths ---------------------
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp"))

;; color themes
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/color-theme-6.6.0/"))

;; org mode
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/org-7.7/lisp"))
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/org-7.7/contrib/lisp"))

;; CEDET
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/cedet-1.0/common"))

;; ECB
(add-to-list 'load-path (concat (get-home-directory) "/site-lisp/ecb-2.40/"))

;; ------------------------------------------------------

;; CEDET
(setq c-initialization-hook)
(add-hook 'c-initialization-hook
  (lambda ()
    (require 'cedet)

    (global-ede-mode 1)                      ; Enable the Project management system
    (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

    (require 'semantic-ia)
    (require 'semantic-gcc)
    (require 'ecb)

    (require 'etags)

    (defun zuza-semantic-ia-fast-jump () (interactive)
      (ring-insert find-tag-marker-ring (point-marker))
      (semantic-ia-fast-jump (point))
    )

    (defun my-cedet-hook ()
      (local-set-key [(control return)] 'semantic-ia-complete-symbol)
      (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
      (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
      (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle);; ?
      (local-set-key "\C-cn" 'eassist-switch-h-cpp)
      (local-set-key "\M-," 'zuza-semantic-ia-fast-jump)
    )

    (add-hook 'c-mode-common-hook 'my-cedet-hook)
  )
)

(run-with-idle-timer 3 nil
  (lambda ()
    (require 'color-theme)
    (set-face-attribute 'default nil :height 75)
  )
)

(eval-after-load "color-theme"
	'(progn
		 (color-theme-initialize)
		 (color-theme-dark-blue2)
	 )
)

(require 'cl)
(setq-default indent-tabs-mode nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((compile-suffix . "-lboost_date_time -lboost_filesystem -lglog -lgflags") (compile-suffix . "-lboost_date_time -lboost_filesystem -lboost_thread") (compile-suffix . "-lboost_date_time -lboost_filesystem"))))
 '(save-abbrevs nil)
)
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; alt + up/down/left/right
(windmove-default-keybindings 'meta)

;; disable the annoying C-w
(put 'kill-region 'disabled t)

;; truncate lines that are longer then the buffer window (don't wrap)
(setq-default truncate-lines t)

(setq-default tab-width 2)
(setq compilation-ask-about-save nil)

(defun org-mode-loader ()
  (interactive)
  (unless (boundp 'org-mode-loaded)
    (setq org-mode-loaded)
    (require 'org-install)
    (condition-case nil
        (require 'org-checklist)   ;; requires a2ps
      (error (message "Org-checklist ERROR"))
    )
  )
  (org-mode)
)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode-loader))
(defmacro safe-var (x)
	"Safely retrieves the variable if it exists. Suppress the error if it doesn't."
	(list 'condition-case 'err
				x
			(list 'error 'nil)
	)
)

(defun kill-all-local-variables-int () (interactive)
  (kill-all-local-variables)
)

(defun revert-all-buffers()
	"Refreshs all open buffers from their respective files"
	(interactive)
	(let* ((list (buffer-list))
				 (buffer (car list)))
		(while buffer
			(if (string-match "\\*" (buffer-name buffer)) 
					(progn
						(setq list (cdr list))
						(setq buffer (car list)))
	      (progn
	        (set-buffer buffer)
	        (revert-buffer t t t)
	        (setq list (cdr list))
	        (setq buffer (car list))))))
	(message "Refreshing open files")
)

(defun equal-split (num)
  "Split the frame into num equal sized sub-windows."
  (interactive "nNumber of subwindows: ")
  (let ( (width (/ (window-width) num)) (cur-win nil) )
    (while (progn (setq cur-win (split-window cur-win width t)) (setq num (1- num)) (> num 1)) )
  )
)

(defun get-compile-command (prefix)
  (if (= prefix 1)
      (concat "g++ " (buffer-name) " -O2 -Wall")
      (concat "g++ " (buffer-name) " -g -ggdb -Wall")
  )
)

(defun save-and-quick-compile (prefix) (interactive "p")
	(save-buffer 0)
	(if (file-readable-p "Makefile")
		(compile (concat "make " (safe-var make-command)))
		(compile (get-compile-command prefix))
	)
)

(defun zuza-quick-run () (interactive)
	(if (file-readable-p "Makefile")
			(async-shell-command "make run")
			(async-shell-command "./a.out")
	)
)

(defun zuza-c++-mode-hook ()
	(local-set-key "\M-2" 'save-and-quick-compile)
	(local-set-key "\M-3" 'zuza-quick-run)
	(local-set-key "\M-n" 'next-error)
	(local-set-key "\M-p" 'previous-error)

	(local-set-key (kbd "<f2>") 'save-and-quick-compile)
	(local-set-key (kbd "<f3>") 'zuza-quick-run)
)

(add-hook 'c++-mode-hook 'zuza-c++-mode-hook)

(defun zuza-close-tmp-buffers ()
"Close all visible windows that are temporary, ie. whose names start with an *"
 (interactive)
  (loop for w in
	(let ((kill-list nil))
	  (loop for w in (window-list) do 
		(let (( name (buffer-name (window-buffer w)) ))
		  (when (string-prefix-p "*" name)
		    (when (not (string= name "*scratch*"))
		      (setq kill-list (cons w kill-list))
		      )
		    )
		  )
		)
	  kill-list
	)
	do (kill-buffer (window-buffer w)) (delete-window w)
 )
)

(global-set-key (kbd "M-2") 'compile)
(global-set-key (kbd "M-4") 'zuza-close-tmp-buffers)

(global-set-key (kbd "<f2>") 'compile)
(global-set-key (kbd "<f4>") 'zuza-close-tmp-buffers)

(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "C-c C-]") 'revert-all-buffers)
(global-set-key (kbd "`") 'dabbrev-expand)

;; Graphviz

(require 'graphviz-dot-mode)
(add-hook 'graphviz-dot-mode-hook
  (lambda ()        
    (local-set-key (kbd "M-3") 'graphviz-dot-preview)
  )
)

;; Python - ropemacs mode - require pymacs and rope
(add-hook 'python-mode-hook 
  (lambda () 
    (when (boundp 'ropemacs-loaded)
      (condition-case nil ;; pymacs & rope could not exist
          (ropemacs-mode)
        (error (message "ropemacs ERROR part 2"))
      )
    )
    (unless (boundp 'ropemacs-loaded)
      (setq ropemacs-loaded)
      (condition-case nil ;; pymacs & rope could not exist
          (progn
            (require 'pymacs)
            (pymacs-load "ropemacs" "rope-")
            (require 'etags)
          )
        (error (message "ropemacs ERROR"))
      )
      (defun zuza-rope-goto-definition ()
        (interactive)
        (ring-insert find-tag-marker-ring (point-marker))
        (rope-goto-definition)
      )
    )
    (local-set-key (kbd "C-c g") 'zuza-rope-goto-definition)
  )
)

;; gnus
(eval-after-load "gnus"
  '(progn
     (setq gnus-select-method '(nnimap "gmail"
       (nnimap-address "imap.gmail.com")
       (nnimap-server-port 993)
       (nnimap-stream ssl))
     )
     (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  )
)

(if (file-readable-p "private.el")
    (load-file "private.el")
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "#FFFF80"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#BBAAFF"))))
)
