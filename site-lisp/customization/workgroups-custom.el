(require 'workgroups)
(workgroups-mode 1)
(setq my-wg-default-file "~/.my.workgroups")
(setq wg-file my-wg-default-file)

(if (file-readable-p my-wg-default-file)
    (wg-load "~/.my.workgroups")
  )
