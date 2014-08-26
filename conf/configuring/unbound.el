;;; Show unbound keys.
;;; Usage: M-x describe-unbound-keys, enter complexity (try 5).

(use-package unbound ; Installed in `my-vendor-dir'.
  :commands describe-unbound-keys)

(provide 'conf/configuring/unbound)
