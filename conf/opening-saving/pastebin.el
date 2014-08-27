;;; Support for the paste.debian.net pastebin.

(use-package debpaste
  :ensure debpaste
  :defer t
  :init

  ;; C-c p -- paste region or buffer.
  (defun my-debpaste-dwim ()
    "If the region is active, call `debpaste-paste-region'. Otherwise, `debpaste-paste-buffer'."
    (interactive)
    (if (use-region-p)
        (call-interactively #'debpaste-paste-region)
      (call-interactively #'debpaste-paste-buffer)))
  (bind-key "C-c p" #'my-debpaste-dwim)

  ;; C-c P i -- display the info associated with last posted paste.
  ;; C-c P d -- delete the paste with a given hash (hash under point is used if available.)
  :bind (("C-c P i" . debpaste-display-posted-info-in-buffer)
         ("C-c P d" . debpaste-delete-paste))

  :config

  ;; Prompt for expiration time and paste language.
  (setq debpaste-prompted-paste-options '(lang expire-sec))

  (setq debpaste-user-name "pkkm"))

(provide 'conf/opening-saving/pastebin)
