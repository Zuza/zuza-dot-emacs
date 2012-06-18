;; This doens't work at all right now

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
