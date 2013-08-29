;;; Popups with possible keys after a prefix key.

(require 'conf/packages)
(package-ensure-installed 'guide-key)
(guide-key-mode 1)

(setq guide-key/guide-key-sequence '("e RET"
                                     "e C-n"
                                     "e @"))

(provide 'conf/other/guide-key)
