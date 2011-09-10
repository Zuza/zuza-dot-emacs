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
