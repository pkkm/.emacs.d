;;; Show unbound keys.
;;; Usage: M-x describe-unbound-keys, enter complexity (try 5).

(autoload
  'describe-unbound-keys
  "unbound" ; Installed in `my-vendor-dir'.
  "Display a list of unbound keystrokes of complexity no greater than MAX.
Keys are sorted by their complexity; `key-complexity' determines it."
  t) ; Interactive.

(provide 'conf/configuring/unbound)
