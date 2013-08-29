;;; Hungry delete -- delete all whitespace before point.

(package-ensure-installed 'hungry-delete)

(global-set-key (kbd "M-<backspace>") #'hungry-delete-backward)
(global-set-key (kbd "M-DEL") #'hungry-delete-backward)

(provide 'conf/operators/hungry-delete)
