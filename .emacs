(defun get-home-directory ()
  (getenv "HOME")
)

(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode nil)
(global-font-lock-mode t)
(column-number-mode 1)
(show-paren-mode 1)

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

(require 'color-theme)
(eval-after-load "color-theme"
	'(progn 
		 (color-theme-initialize)
		 (color-theme-dark-blue2)
	 )
)

(require 'org-install)
(require 'cl)
(require 'graphviz-dot-mode)
(require 'cedet)

(setq-default indent-tabs-mode nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; alt + up/down/left/right
(windmove-default-keybindings 'meta)

;; disable the annoying C-w
(put 'kill-region 'disabled t)

;; truncate lines that are longer then the buffer window (don't wrap)
(setq-default truncate-lines t)

(setq default-tab-width 2)
(setq compilation-ask-about-save nil)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(defmacro safe-var (x)
	"Safely retrieves the variable if it exists. Suppress the error if it doesn't."
	(list 'condition-case 'err
				x
			(list 'error 'nil)
	)
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

;; Graphviz
(global-set-key (kbd "M-2") 'compile)
(global-set-key (kbd "M-4") 'zuza-close-tmp-buffers)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

(add-hook 'graphviz-dot-mode-hook
      (lambda () (local-set-key (kbd "M-3") 'graphviz-dot-preview))
)

;; CEDET
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

(require 'semantic-ia)
(require 'semantic-gcc)
(require 'ecb) 

(load "etags")

(defun zuza-semantic-ia-fast-jump () (interactive)
	(ring-insert find-tag-marker-ring (point-marker))
	(semantic-ia-fast-jump (point))
)

(defun kill-all-local-variables-int () (interactive)
	(kill-all-local-variables)
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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((compile-suffix . "-lboost_date_time -lboost_filesystem -lglog -lgflags") (compile-suffix . "-lboost_date_time -lboost_filesystem -lboost_thread") (compile-suffix . "-lboost_date_time -lboost_filesystem"))))
 '(save-abbrevs nil))

(ede-cpp-root-project "boost_projekt"
 	        :name "Moj boost projekt"
                :file "/home/gzuzic/Downloads/boost_1_47_0/INSTALL"
                :include-path '("/"
                               )
                :system-include-path '("/usr/include"
                                      "/usr/local/include"
                                       )
)

(ede-cpp-root-project "boost_fajlovi"
 	        :name "Moji fajlici"
                :file "/home/gzuzic/work/boost/mpl.cpp"
                :include-path '("/home/gzuzic/Downloads/boost_1_47_0/"
                               )
)


