;; ------------ Color theme config ------------
(if (window-system)
    (progn
      (require 'color-theme)
      (color-theme-initialize)
      (color-theme-dark-blue2)
      (set-face-attribute 'default nil :height 80)
      )
  )
