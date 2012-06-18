(defun zuza-perl-compile ()
  (interactive)
  (async-shell-command (concat "perl -c " (buffer-name)))
)

(defun zuza-perl-run ()
  (interactive)
  (async-shell-command (concat "perl " (buffer-name)))
)

(add-hook 'perl-mode-hook         
  (lambda ()
    (local-set-key (kbd "M-2") 'zuza-perl-compile)
    (local-set-key (kbd "M-3") 'zuza-perl-run)
  )
)
