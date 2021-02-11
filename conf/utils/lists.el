;;; Utilities for lists. -*- lexical-binding: t -*-

(defun recar (cell new-car)
  "Like `setcar', but creates a new cons cell instead of modifying CELL."
  (cons new-car (cdr cell)))

(defun list-to-vector (list)
  "Convert LIST to a vector."
  (vconcat [] list))

(defun vector-to-list (vector)
  "Convert VECTOR to a list."
  (append vector '()))

(use-package s :ensure t :commands s-join)
(defun join-nonempty (separator &rest parts)
  "Return a string with nonempty (not nil or \"\") elements of PARTS joined with SEPARATOR."
  (declare (indent defun))
  (s-join separator (delq nil (delete "" parts))))

(defun add-to-list-after (list-var element after-what)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
ELEMENT will be added after the first occurrence of AFTER-WHAT,
or at the beginning if AFTER-WHAT isn't in the list. Comparisons
are done with `equal'."
  (unless (member element (symbol-value list-var))
    (let ((after-position (-elem-index after-what (symbol-value list-var))))
      (set list-var
           (-insert-at (if after-position (1+ after-position) 0)
                       element
                       (symbol-value list-var))))))

(provide 'conf/utils/lists)
