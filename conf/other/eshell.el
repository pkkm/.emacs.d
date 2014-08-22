;;; Eshell -- Emacs shell.

(use-package eshell ; Bundled with Emacs.
  :defer t
  :config

  ;; Keep savefiles in `my-savefile-dir'/eshell.
  (setq eshell-directory-name
        (expand-file-name "eshell" my-savefile-dir)))

(provide 'conf/other/eshell)
