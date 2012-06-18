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

(defmacro safe-var (x)
	"Safely retrieves the variable if it exists. Suppress the error if it doesn't."
	(list 'condition-case 'err
				x
			(list 'error 'nil)
	)
)
