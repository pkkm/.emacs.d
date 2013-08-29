;;; Operators connected with the clipboard.
;;;   Destroy -- like `d', but without changing the clipboard.
;;;   Replace -- replace selected text with text from clipboard.

(require 'conf/evil)

(evil-define-operator evil-destroy (start end type register yank-handler)
  (interactive "")
  (cond
   ((eq type 'block)
    (delete-rectangle start end))
   ((and (eq type 'line)
         (= (point-max) end)
         (/= (point-min) start))
    (delete-region (1 - start) end))
   (t
    (delete-region start end)))
  (when (eq type 'line)
    (back-to-indentation)))

(evil-define-operator evil-destroy-replace (start end type register yank-handler)
  (evil-destroy start end type register yank-handler)
  (evil-paste-before 1 register))

(provide 'conf/operators/clipboard)
