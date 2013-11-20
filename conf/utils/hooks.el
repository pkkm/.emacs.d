;;; Hook utilities.

(defun add-hooks (hooks function)
  "Like `add-hook', but accepts multiple hooks (as a list of symbols)."
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defmacro add-one-shot-hook (hook function)
  "Add FUNCTION to HOOK. Remove it after its first execution."
  (let ((wrapper-function (make-symbol "add-one-shot-hook--wrapper-function")))
    `(progn
       (eval-when-compile (require 'cl)) ; Used: lexical-let.
       (lexical-let ((hook* ,hook) ; Create a closure.
                     (function* ,function))
         (defun ,wrapper-function ()
           "Wrapper function that will be executed only once, and then removed from the hook."
           (unwind-protect
               (funcall function*)
             (remove-hook hook* ',wrapper-function)))
         (add-hook hook* ',wrapper-function)))))

(defun add-one-shot-hooks (hooks function)
  "Like `add-one-shot-hook', but accepts multiple hooks (as a list of symbols)."
  (mapc (lambda (hook)
          (add-one-shot-hook hook function))
        hooks))

(provide 'conf/utils/hooks)
