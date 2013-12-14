;;; Popups with possible keys after a prefix key.

(require 'conf/packages)
(package-ensure-installed 'guide-key)
(guide-key-mode 1)

(setq guide-key/guide-key-sequence
      '("e RET" "e C-n"))

;; Don't show the minor mode in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'guide-key-mode "")

(provide 'conf/other/guide-key)
