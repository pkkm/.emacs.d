;;; Hungry delete -- delete all whitespace before point.

(require 'conf/packages)
(require 'conf/evil)

(use-package hungry-delete
  :ensure hungry-delete
  :commands (hungry-delete-forward hungry-delete-backward)
  :init
  (dolist (keymap (list evil-normal-state-map evil-insert-state-map))
    (bind-key "M-<backspace>" #'hungry-delete-backward keymap)
    (bind-key "M-DEL" #'hungry-delete-backward keymap)))

(provide 'conf/operators/hungry-delete)
