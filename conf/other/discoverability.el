;;; Discoverability features.

;; When I press a prefix key, pop up a list of commands.
(use-package guide-key
  :ensure guide-key
  :defer t
  :diminish guide-key-mode
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

(provide 'conf/other/discoverability)
