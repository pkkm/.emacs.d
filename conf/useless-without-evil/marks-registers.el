;;; Jumping to mark/register.
;; Emacs registers are global, in contrast to Evil's buffer-local markers. They also can be used for storing window configurations, file names, etc.

(with-eval-after-load 'evil
  ;; ' -- go to a mark (instead of the mark's line).
  (bind-key "'" #'evil-goto-mark evil-motion-state-map)

  ;; M -- store the point position in an Emacs register
  ;; ` -- jump to a register.
  (bind-key "M" #'point-to-register evil-motion-state-map)
  (bind-key "`" #'jump-to-register evil-motion-state-map))

(provide 'conf/useless-without-evil/marks-registers)
