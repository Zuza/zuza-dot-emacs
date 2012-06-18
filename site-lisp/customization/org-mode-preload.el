;; Not the best solution in the world, should probably create an alias somehow (fset doesn't work)

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
