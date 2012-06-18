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
