;;; Ignore some or all `message's from a piece of code.

(require 'cl-lib) ; Used: cl-letf.

(defmacro ignore-messages (&rest body)
  "Ignore `message's from the code in BODY (replace calls to `message' with `format')."
  `(cl-letf (((symbol-function 'message) 'format))
     ,@body))

(defmacro ignore-specific-messages (messages-to-ignore &rest body)
  "When `message' is called with one of MESSAGES-TO-IGNORE, ignore it (replace calls to `message' with `format')."
  (let ((original-message-function (symbol-function 'message)))
    `(cl-letf (((symbol-function 'message)
                (lambda (&rest args)
                  (if (member (car args) ,messages-to-ignore)
                      (apply 'format args)
                    (apply ,original-message-function args)))))
       ,@body)))

(provide 'conf/utils/ignore-messages)
