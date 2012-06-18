;; ------------ All sorts of preferences ------------
(fset 'yes-or-no-p 'y-or-n-p)
(set-scroll-bar-mode nil)
(global-font-lock-mode t)
(column-number-mode 1)
(show-paren-mode 1)
(tool-bar-mode 0)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t) ; truncate lines that are longer then the buffer window (don't wrap)
(setq-default tab-width 2)
(setq compilation-ask-about-save nil)
;; (menu-bar-mode 0)

;; ------------ Navigation --------------
(windmove-default-keybindings 'meta) ; alt + up/down/left/right

;; ------------ Color theme config ------------
(run-with-idle-timer 3 nil
  (lambda ()
    (require 'color-theme)
    (set-face-attribute 'default nil :height 75)
  )
)

(eval-after-load "color-theme"
	'(progn
		 (color-theme-initialize)
		 (color-theme-dark-blue2)
	 )
)

;; ------------ Disabling ------------
(put 'kill-region 'disabled t) ; disable the annoying C-w
(put 'suspend-tty 'disabled t) ; disable the minimizations
(put 'suspend-emacs 'disabled t)
(put 'suspend-frame 'disabled t)

;; ----------- Keys ------------------
(global-set-key (kbd "M-2") 'compile)
(global-set-key (kbd "<f2>") 'compile)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "C-c C-]") 'revert-all-buffers)
(global-set-key (kbd "`") 'dabbrev-expand)