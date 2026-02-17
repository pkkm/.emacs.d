;;; Utilities for key sequences and keymaps. -*- lexical-binding: t -*-


;;; Key sequences.

(defun concat-keys (key-1 key-2)
  "Concatenate key sequences KEY-1 and KEY-2.
Each argument can be a string or vector, but the output is always a vector."
  (vconcat key-1 key-2))


;;; Keymaps.

(defun clear-keymap (keymap)
  "Delete all bindings in KEYMAP.
The intuitive solution, (setq keymap-name (make-sparse-keymap)), doesn't work because Emacs accesses the keymap by pointer instead of name."
  (setcdr keymap nil))

(defun keymap-to-key-binding-alist (keymap)
  "Association list of all bindings in KEYMAP as key sequences (instead of events):
  '((key . function) ...)
Flattens nested keymaps."
  (let ((result '()))
    (map-keymap
     (lambda (event binding)
       (let ((key (vector event)))
         (if (keymapp binding)
             ;; Recursively flatten nested keymaps.
             (dolist (sub-binding (keymap-to-key-binding-alist binding))
               (let ((sub-key (car sub-binding))
                     (sub-func (cdr sub-binding)))
                 (push (cons (concat-keys key sub-key) sub-func) result)))
           ;; Add direct keybinding.
           (setq result (cons (cons key binding) result)))))
     (keymap-canonicalize keymap)) ; Canonicalize the keymap, so that when a binding shadows another, only the one in effect is returned.
    result))

(require 'cl-lib) ; Used: cl-destructuring-bind.
(defun map-key-sequences-in-keymap (keymap function)
  "Execute FUNCTION for each non-prefix binding in KEYMAP, passing the key and the function bound to it."
  (dolist (key-and-binding-cell (keymap-to-key-binding-alist keymap))
    (cl-destructuring-bind (key . binding) key-and-binding-cell
      (funcall function key binding))))

(defun variables-with-value (value)
  "Return a list of symbols whose value is `eq' to VALUE."
  (let ((result '()))
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                           (eq (symbol-value symbol) value))
                  (push symbol result))))
    result))

(defun keymaps-with-key (key &optional display-in-buffer-p)
  "Returns a list of names of keymaps that have KEY defined in them."
  (interactive "kDisplay keymaps with this key bound: \np")
  (let ((keymaps (list)))
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                           (keymapp (symbol-value symbol))
                           (lookup-key (symbol-value symbol) key))
                  (push symbol keymaps))))
    (when display-in-buffer-p
      (with-current-buffer (get-buffer-create "*Keymaps with key*")
        (erase-buffer)
        (insert (concat "Keymaps with key " (key-description key) " bound in them:\n\n"))
        (dolist (keymap-symbol (sort keymaps #'string-lessp))
          (insert (format "%s\n" keymap-symbol)))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))
    keymaps))


(provide 'conf/utils/keys)
