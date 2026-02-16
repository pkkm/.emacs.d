;;; Hook utilities. -*- lexical-binding: t -*-

(defun add-hooks (hooks function)
  "Like `add-hook', but accepts multiple hooks (as a list of symbols)."
  (dolist (hook hooks)
    (add-hook hook function)))

;; Currently unused.
(defun add-one-shot-hook (hook function)
  "Add FUNCTION to HOOK. Remove it after its first execution."
  (letrec ((temp-hook (lambda (&rest _)
                        (unwind-protect
                            (funcall function)
                          (remove-hook hook temp-hook)))))
    (add-hook hook temp-hook)))

(provide 'conf/utils/hooks)
