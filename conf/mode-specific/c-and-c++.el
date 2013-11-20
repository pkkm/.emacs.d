;;; C and C++.

;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-insinuate 'c 'c++)
(add-hook 'c-mode-common-hook #'enable-indent-tabs-mode)

(add-one-shot-hook 'c-mode-common-hook
                   (lambda ()
                     ))

(provide 'conf/mode-specific/c-and-c++)
