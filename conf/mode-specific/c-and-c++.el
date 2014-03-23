;;; C and C++.

;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-insinuate 'c 'c++)
(add-hook 'c-mode-common-hook #'enable-indent-tabs-mode)

;; Make the default indentation style Linux instead of GNU.
;; This variable is defined after c-mode is loaded, but it needs to be set before c-mode's hooks run. Because of this, it'd be hard to modify it (instead of setting it) in the init file.
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

(add-one-shot-hook 'c-mode-common-hook
                   (lambda ()
                     ))

(provide 'conf/mode-specific/c-and-c++)
