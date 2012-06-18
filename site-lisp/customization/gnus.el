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
