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
    ;; Make windmove work in org-mode:
    (add-hook 'org-metaup-hook 'windmove-up) 
    (add-hook 'org-metaleft-hook 'windmove-left)
    (add-hook 'org-metadown-hook 'windmove-down)
    (add-hook 'org-metaright-hook 'windmove-right)
  )
  (org-mode)
)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode-loader))
