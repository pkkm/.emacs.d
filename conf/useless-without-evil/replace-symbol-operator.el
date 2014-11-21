;;; An operator for replacing the symbol under point in an user-supplied part of buffer.
;; Especially useful with C-M-h (mark-defun).

(require 'cl-lib)
(evil-define-operator evil-replace-symbol (beg end)
  "Build an Evil command for replacing a symbol in the supplied part of the buffer.
When called without region and there's symbol at point, use it."
  :type line ; Takes whole lines. Evil's ":s" command works on whole lines anyway.
  :move-point nil ; Don't go to `beg' before executing (this would mess with `thing-at-point').
  (cl-flet ((ex-before-after (before after) ; Prompt for an Ex command, with point between BEFORE and AFTER.
                             (evil-ex (cons (concat before after) (1+ (length before)))))) ; When the argument is (STRING . POSITION), point is placed at one-indexed POSITION (documented in `read-from-minibuffer').
    (let ((symbol (thing-at-point 'symbol t)))
      (if (and symbol (not (use-region-p)))
          ;; When we were called as an operator with a symbol at point, insert the symbol as the text to replace.
          (let ((replaced (concat (rx symbol-start) symbol (rx symbol-end)))
                (line-range (concat (number-to-string (line-number-at-pos beg)) ","
                                    (number-to-string (line-number-at-pos end)))))
            (ex-before-after (concat line-range "s/" replaced "/")
                             "/g"))
        ;; When we were called in visual state or there's no symbol at point, insert an empty place for a symbol.
        (let ((range (if (use-region-p) "'<,'>" "")))
          (ex-before-after (concat range "s/" (rx symbol-start))
                           (concat (rx symbol-end) "//g")))))))

(bind-key "Q" #'evil-replace-symbol evil-normal-state-map)
(bind-key "Q" #'evil-replace-symbol evil-visual-state-map)

(provide 'conf/useless-without-evil/replace-symbol-operator)
