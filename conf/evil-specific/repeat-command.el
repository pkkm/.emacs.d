;;; Repeat command with RET.

(with-eval-after-load 'evil
  (bind-key "g SPC" #'repeat evil-motion-state-map))

(provide 'conf/evil-specific/repeat-command)
