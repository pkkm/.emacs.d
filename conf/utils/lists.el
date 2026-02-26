;;; Utilities for lists. -*- lexical-binding: t -*-

(defun join-nonempty (separator &rest parts)
  "Return a string with nonempty (not nil or \"\") elements of PARTS joined with SEPARATOR."
  (declare (indent defun))
  (string-join (delq nil (delete "" parts)) separator))

(defun add-to-list-after (list-var element after-what)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
ELEMENT will be added after the first occurrence of AFTER-WHAT,
or at the beginning if AFTER-WHAT isn't in the list. Comparisons
are done with `equal'."
  (let ((lst (symbol-value list-var)))
    (unless (member element lst)
      (if-let ((tail (member after-what lst)))
          (setcdr tail (cons element (cdr tail)))
        (set list-var (cons element lst))))))

(provide 'conf/utils/lists)
