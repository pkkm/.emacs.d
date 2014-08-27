;;; TRAMP -- transparent manipulating of remote files.

(use-package tramp
  :defer t
  :config

  ;; Perform multiple hops like this:
  ;;   /ssh:root@server|su:user@localhost:/path/to/file
  (setq tramp-save-ad-hoc-proxies nil) ; Don't permanently save such multi-hops.

  ;; Automatic multi-hop proxies.
  (setq tramp-default-proxies-alist
        ;; When attempting to open a file on llaesh as a non-root user (e.g. "/su:user@llaesh:/file"), tunnel through "ssh root@llaesh".
        `((,(rx string-start "llaesh" string-end) (rx string-start "root" string-end) nil) ; Don't tunnel for root (to avoid infinite recursion).
          (,(rx string-start "llaesh" string-end) nil "/ssh:root@llaesh:"))) ; Tunnel for everyone else.

  ;; Savefile.
  (setq tramp-persistency-file-name
        (expand-file-name "tramp" my-savefile-dir)))

(provide 'conf/opening-saving/tramp)
