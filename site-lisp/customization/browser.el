(let ((chrome (executable-find "google-chrome")))
  (if chrome
    (progn (setq browse-url-browser-function (quote browse-url-generic))
           (setq browse-url-generic-program "google-chrome")
           )
    )
  )
