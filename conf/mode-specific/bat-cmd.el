;;; DOS batch and Windows CMD scripts.

(require 'conf/packages)

(use-package dos ; Installed in `my-vendor-dir'.
  :commands dos-mode)

(provide 'conf/mode-specific/bat-cmd)
