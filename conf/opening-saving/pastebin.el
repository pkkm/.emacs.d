;;; Support for the paste.debian.net pastebin.

(require 'conf/packages)
(package-ensure-installed 'debpaste)

;; C-c p -- paste region or buffer.
(defun my-debpaste-dwim ()
  "If the region is active, call `debpaste-paste-region'. Otherwise, `debpaste-paste-buffer'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'debpaste-paste-region)
    (call-interactively #'debpaste-paste-buffer)))
(global-set-key (kbd "C-c p") #'my-debpaste-dwim)

;; C-c P i -- display the info associated with last posted paste.
;; C-c P d -- delete the paste with a given hash (hash under point is used if available.)
(global-set-key (kbd "C-c P i") #'debpaste-display-posted-info-in-buffer)
(global-set-key (kbd "C-c P d") #'debpaste-delete-paste)

;; Prompt for expiration time and paste language.
(setq debpaste-prompted-paste-options '(lang expire-sec))

(setq debpaste-user-name "pkkm")

(provide 'conf/opening-saving/pastebin)
