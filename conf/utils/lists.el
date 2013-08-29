;;; Utilities for lists.

(defun recar (cell new-car)
  "Like `setcar', but creates a new cons cell instead of modifying CELL."
  (cons new-car (cdr cell)))

(defun list-to-vector (list)
  "Convert LIST to a vector."
  (vconcat [] list))

(defun vector-to-list (vector)
  "Convert VECTOR to a list."
  (append vector '()))

(provide 'conf/utils/lists)
