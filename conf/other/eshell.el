;;; Eshell -- Emacs shell.

;; Keep savefiles in `my-savefile-dir'/eshell.
(setq eshell-directory-name
      (expand-file-name "eshell" my-savefile-dir))

(provide 'conf/other/eshell)
