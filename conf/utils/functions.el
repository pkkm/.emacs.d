;;; Utilities for functions. -*- lexical-binding: t -*-

(defmacro define-interactive-wrapper (name arglist copy-from &optional docstring &rest body)
  "Define a function that uses the `interactive' spec from another function.
For creating future-proof passthroughs."
  (declare (doc-string 4) (indent 3))
  `(defun ,name ,arglist
     ,@(when (stringp docstring) (list docstring))
     ;; See <https://emacs.stackexchange.com/a/19242>.
     (interactive (advice-eval-interactive-spec
                   (cadr (interactive-form #',copy-from))))
     ;; Replace `(diw-apply-original-fun args)' with an application of COPY-FROM that has the right interactivity.
     ,@(-tree-map-nodes
        (lambda (node) (and (consp node) (eq (car node) 'diw-apply-original-fun)))
        (lambda (node)
          `(apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
                  #',copy-from ,@(cdr node)))
        (if (stringp docstring) body (cons docstring body)))))

(provide 'conf/utils/functions)
