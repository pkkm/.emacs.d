;;; Popups with possible keys after a prefix key.

(require 'conf/packages)
(package-ensure-installed 'guide-key)
(guide-key-mode 1)

(setq guide-key/idle-delay 0.5)

(setq guide-key/recursive-key-sequence-flag t) ; If `guide-key' is enabled for a keymap, also enable it for its subkeymaps.
(setq guide-key/guide-key-sequence
      '("e" "C-x" "SPC" "C-c" "e RET" "C-x RET" "e C-n" "C-x n"))

;; Don't show the minor mode in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'guide-key-mode "")

(provide 'conf/other/guide-key)
