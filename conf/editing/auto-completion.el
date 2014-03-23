;;; Auto-completion.

(require 'conf/packages)
(package-ensure-installed 'auto-complete)

;; Savefile.
(setq ac-comphist-file
      (expand-file-name "auto-complete-history" my-savefile-dir))

(global-auto-complete-mode 1)

(provide 'conf/editing/auto-completion)
