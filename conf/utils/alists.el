;;; Utilities for alists (associative lists).

(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already
present, replace it with ELT-CONS unless NO-REPLACE is non-nil.
Otherwise, add ELT-CONS to the front of the alist. The test for
presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (setcdr existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))

(provide 'conf/utils/alists)
