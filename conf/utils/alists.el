;;; Utilities for alists (associative lists).

(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add ELT-CONS to the alist with the name ALIST-VAR if it isn't there yet.
If an element with the same car as ELT-CONS is not present, add ELT-CONS to the front of the alist. Otherwise, replace the conflicting element with ELT-CONS unless NO-REPLACE is non-nil. The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (setcdr existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))

(provide 'conf/utils/alists)
