;;; An operator for replacing the symbol under point in an user-supplied part of buffer. -*- lexical-binding: t -*-
;; Especially useful with C-M-h (mark-defun).

(with-eval-after-load 'evil
  (defun my-evil-ex-before-after (before after)
    "Prompt for an Ex command, with point between BEFORE and AFTER."
    (evil-ex (cons (concat before after) (1+ (length before)))))

  (evil-define-operator evil-replace-symbol (beg end)
    "Build an Evil command for replacing a symbol in the supplied part of the buffer.
When called without region and there's symbol at point, use it."
    :type line ; Takes whole lines. Evil's ":s" command works on whole lines anyway.
    :move-point nil ; Don't go to `beg' before executing (this would mess with `thing-at-point').
    (let ((symbol (thing-at-point 'symbol t)))
      (if (and symbol (not (evil-visual-state-p)))
          ;; When we were called as an operator with a symbol at point, insert the symbol as the text to replace.
          (my-evil-ex-before-after
           (format "%d,%ds/%s/"
                   (line-number-at-pos beg)
                   (line-number-at-pos (1- end)) ; Subtract 1 since with `:type line', `end' is the start of the line after the selected block of lines.
                   (concat (rx symbol-start) (regexp-quote symbol) (rx symbol-end)))
           "/gI") ; I -- turn off case insensitivity.
        ;; When we were called in visual state or there's no symbol at point, insert an empty place for a symbol.
        (let ((range (if (evil-visual-state-p) "'<,'>" "")))
          (my-evil-ex-before-after (concat range "s/" (rx symbol-start))
                                   (concat (rx symbol-end) "//gI"))))))

  (evil-define-key '(normal visual) 'global (kbd "Q") #'evil-replace-symbol))

(provide 'conf/evil-specific/replace-symbol-operator)
