(autoload 'js2-mode "js2-mode" nil t nil)
(add-to-list 'auto-mode-alist '("[.]js$" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)
            ))

