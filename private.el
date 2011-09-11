;; Personal configuration goes here

(ede-cpp-root-project "boost_projekt"
 	        :name "Moj boost projekt"
                :file "/home/gzuzic/Downloads/boost_1_47_0/INSTALL"
                :include-path '("/"
                               )
                :system-include-path '("/usr/include"
                                      "/usr/local/include"
                                       )
)

(ede-cpp-root-project "boost_fajlovi"
 	        :name "Moji fajlici"
                :file "/home/gzuzic/work/boost/mpl.cpp"
                :include-path '("/home/gzuzic/Downloads/boost_1_47_0/"
                               )

)

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
