;;; Show unbound keys.
;;; Usage: M-x describe-unbound-keys, enter complexity (try 5).

(require 'conf/packages)

(use-package unbound ; Installed in `my-vendor-dir'.
  :commands describe-unbound-keys)

(provide 'conf/configuring/unbound)
