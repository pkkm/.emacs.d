;;; Disable minimizing graphical frames (bound to C-z, C-x C-z, etc.).
;; Every time I did this, it was by accident, and led to lost windows (i3 hides minimized windows, with no way to restore them).

(defadvice iconify-frame (around disable-iconifying activate)
  (message "Not minimizing frame."))

(provide 'conf/other/disable-suspending)
