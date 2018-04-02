;;; Support for the paste.debian.net pastebin. -*- lexical-binding: t -*-

(use-package debpaste
  :ensure t
  :init

  (defun my-debpaste-region-or-buffer ()
    "If the region is active, call `debpaste-paste-region'. Otherwise, `debpaste-paste-buffer'."
    (interactive)
    (if (use-region-p)
        (call-interactively #'debpaste-paste-region)
      (call-interactively #'debpaste-paste-buffer)))
  (bind-key "C-c P p" #'my-debpaste-region-or-buffer)

  :bind (("C-c P i" . debpaste-display-posted-info-in-buffer) ; Display info about last paste.
         ("C-c P d" . debpaste-delete-paste)) ; Delete paste with a given hash (default: hash at point).

  :config

  ;; Prompt for expiration time and paste language.
  (setq debpaste-prompted-paste-options '(lang expire-sec))

  (setq debpaste-user-name "pkkm"))

(provide 'conf/other/pastebin)
