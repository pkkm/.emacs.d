;;; Repeat command with RET.

(with-eval-after-load 'evil
  (bind-key "RET" #'repeat evil-motion-state-map))

(provide 'conf/evil-specific/repeat-command)
