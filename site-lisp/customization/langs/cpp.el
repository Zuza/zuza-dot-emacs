(defcustom make-compile-command nil
  "If the makefile is present, use this as the make argument for compilation"
)
(defcustom make-run-command nil
  "If the makefile is present, use this as the make argument for running"
)

(defun get-compile-command (prefix)
  (if (= prefix 1)
      (concat "g++ " (buffer-name) " -O2 -Wall -o " (file-name-sans-extension (buffer-name)))
      (concat "g++ " (buffer-name) " -g -ggdb -Wall -o" (file-name-sans-extension (buffer-name)))
  )
)

(defun save-and-quick-compile (prefix) (interactive "p")
	(save-buffer 0)
	(if (file-readable-p "makefile")
		(compile (concat "make " make-compile-command))
		(compile (get-compile-command prefix))
	)
)

(defun zuza-quick-run () (interactive)
	(if (file-readable-p "makefile")
			(async-shell-command (concat "make run " make-run-command) )
      (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) ))
	)
)

(defun zuza-tc-run-0 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 0"))
)
(defun zuza-tc-run-1 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 1"))
)
(defun zuza-tc-run-2 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 2"))
)
(defun zuza-tc-run-3 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 3"))
)
(defun zuza-tc-run-4 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 4"))
)
(defun zuza-tc-run-5 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 5"))
)
(defun zuza-tc-run-6 () (interactive)
  (async-shell-command (concat "./" (file-name-sans-extension (buffer-name)) " 6"))
)

(defun zuza-c++-mode-hook ()
	(local-set-key "\M-2" 'save-and-quick-compile)
	(local-set-key "\M-3" 'zuza-quick-run)
	(local-set-key "\M-n" 'next-error)
	(local-set-key "\M-p" 'previous-error)

	(local-set-key "\M-4" 'zuza-tc-run-0)
	(local-set-key "\M-5" 'zuza-tc-run-1)
	(local-set-key "\M-6" 'zuza-tc-run-2)
	(local-set-key "\M-7" 'zuza-tc-run-3)
	(local-set-key "\M-8" 'zuza-tc-run-4)
	(local-set-key "\M-9" 'zuza-tc-run-5)
	(local-set-key "\M-0" 'zuza-tc-run-6)

	(local-set-key (kbd "<f2>") 'save-and-quick-compile)
	(local-set-key (kbd "<f3>") 'zuza-quick-run)
)

(add-hook 'c++-mode-hook 'zuza-c++-mode-hook)

(c-add-style "zuza" 
  '("gnu"
    (c-offsets-alist
     ;; for the full list check the doc for (describe-variable 'c-offsets-alist))
     ;; also, C-x C-o is helpful for quickly tracking the indentation rules
     (arglist-intro . +)
     (arglist-close . 0)
    )
   )
)

(setq c-default-style "zuza")





