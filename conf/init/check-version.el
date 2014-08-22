;;; Display an error if Emacs is older than 24.3.

(when (version< emacs-version "24.3")
  (error (concat "This config requires Emacs 24.3+. Current version: " emacs-version)))

(provide 'conf/init/check-version)
