;;; Popups with possible keys after a prefix key.

(require 'conf/packages)

(use-package guide-key
  :ensure guide-key
  :diminish guide-key-mode
  :commands guide-key-mode
  :init
  (guide-key-mode 1)
  :config
  (setq guide-key/idle-delay 0.5)
  (setq guide-key/recursive-key-sequence-flag t) ; If `guide-key' is enabled for a keymap, also enable it for its subkeymaps.
  (setq guide-key/guide-key-sequence
        '("e" "C-x"
          "SPC" "C-c"
          "e RET" "C-x RET"
          "e C-n" "C-x n")))

(provide 'conf/other/guide-key)
