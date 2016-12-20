;;; Ignore some or all `message's from a piece of code. -*- lexical-binding: t -*-

(require 'cl-lib) ; Used: cl-letf.

(defmacro ignore-messages (&rest body)
  "Ignore `message's from the code in BODY (replace calls to `message' with `format')."
  `(cl-letf (((symbol-function 'message) 'format))
     ,@body))

(defmacro ignore-specific-messages (messages-to-ignore &rest body)
  "When `message' is called with one of MESSAGES-TO-IGNORE, ignore it (replace calls to `message' with `format')."
  (let ((original-message-function-symbol
         (make-symbol "ignore-specific-messages--original-message-function")))
    `(let ((,original-message-function-symbol (symbol-function 'message)))
       (cl-letf (((symbol-function 'message)
                  (lambda (&rest args)
                    (if (member (car args) ,messages-to-ignore)
                        (apply 'format args)
                      (apply ,original-message-function-symbol args))))) ; Extra careful with such things: byte-compilation's inlining is sometimes too literal, for example (apply #<subr message> ...), which causes "Symbol's function definition is void" or "Invalid read syntax".
         ,@body))))

(provide 'conf/utils/ignore-messages)
