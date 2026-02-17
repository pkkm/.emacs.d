;;; Utilities for lists. -*- lexical-binding: t -*-

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
    (let* ((lst (symbol-value list-var))
           (after-position (seq-position lst after-what)))
      (set list-var
           (-insert-at (if after-position (1+ after-position) 0)
                       element
                       (symbol-value list-var))))))

(provide 'conf/utils/lists)
