;;; Show unbound keys. -*- lexical-binding: t -*-
;; Usage: M-x describe-unbound-keys, enter complexity (try 5).

(use-package unbound ; Installed in `my-vendor-dir' because the MELPA package is fetched from the wiki, which is insecure (as of 2017-06).
  :commands describe-unbound-keys)

(provide 'conf/other/unbound-keys)
