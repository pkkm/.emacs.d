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

(defun interpose-nonempty (separator &rest parts)
  "Return a list with nonempty (not nil or \"\") elements of PARTS separated with SEPARATOR."
  (declare (indent defun))
  (-interpose separator (delq nil (delete "" parts))))

(use-package s :ensure t :commands s-join)
(defun join-nonempty (separator &rest parts)
  "Return a string with nonempty (not nil or \"\") elements of PARTS joined with SEPARATOR."
  (declare (indent defun))
  (s-join separator (delq nil (delete "" parts))))

(provide 'conf/utils/lists)
