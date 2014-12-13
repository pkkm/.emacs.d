;;; Disable suspending frame with C-z.
;; Every time I used this, it was by accident, and led to lost windows when using i3 -- it hides minimized windows, with no way to restore them.

(bind-key "C-z" nil)

(provide 'conf/other/disable-suspending)
