;;; Utilities for key sequences and keymaps. -*- lexical-binding: t -*-


;;; Key sequences.

(defun key-as-vector (key)
  "Return the key sequence KEY represented as a vector."
  (cond
   ((stringp key) (string-to-vector key))
   ((vectorp key) key)
   (t (error "KEY is not a key sequence (vector or string)"))))

(defun concat-keys (key-1 key-2)
  "Concatentate key sequences KEY-1 and KEY-2.
The result is a vector if KEY-1 or KEY-2 is a vector. Otherwise it's a string."
  (cond
   ((or (vectorp key-1) (vectorp key-2))
    (vconcat (key-as-vector key-1) (key-as-vector key-2)))
   ((and (stringp key-1) (stringp key-2))
    (concat key-1 key-2))
   ((not (or (vectorp key-1) (stringp key-1)))
    (error "KEY-1 is not a key sequence (vector or string)"))
   ((not (or (vectorp key-2) (stringp key-2)))
    (error "KEY-2 is not a key sequence (vector or string)"))))


;;; Keymaps.

(defun clear-keymap (keymap)
  "Delete all bindings in KEYMAP.
The intuitive solution, (setq keymap-name (make-sparse-keymap)), doesn't work because Emacs accesses the keymap by pointer instead of name."
  (setcdr keymap nil))

(require 'conf/utils/lists) ; Used: recar.
(defun prepend-keys-in-key-binding-alist (key key-binding-alist)
  "Prepend the key sequences in KEY-BINDING-ALIST with KEY.
KEY-BINDING-ALIST should be a keymap-like alist of (key . binding)."
  (mapcar (lambda (key-and-binding-cell)
            (recar key-and-binding-cell
                   (concat-keys key (car key-and-binding-cell))))
          key-binding-alist))

(defun keymap-to-key-binding-alist (keymap)
  "Association list of all bindings in KEYMAP as key sequences (instead of events):
  '((key . function) ...)
Flattens nested keymaps."
  (let ((result '()))
    (map-keymap
     (lambda (event binding)
       (let ((key (vector event)))
         (if (keymapp binding)
             ;; Recurse to get an alist of the keys and bindings in `keymap'.
             ;; Prepend `key' to the key sequences in this alist.
             ;; Add the result to `result'.
             (let ((nested-keymap-key-binding-alist
                    (prepend-keys-in-key-binding-alist key
                     (keymap-to-key-binding-alist binding))))
               (setq result (append nested-keymap-key-binding-alist result)))
           ;; Add (key . binding) to `result'.
           (setq result (cons (cons key binding) result)))))
     (keymap-canonicalize keymap)) ; Canonicalize the keymap, so that when a binding shadows another, only the one in effect is returned.
    result))

(require 'cl-lib) ; Used: cl-destructuring-bind.
(defun map-key-sequences-in-keymap (keymap function)
  "Execute FUNCTION for each non-prefix binding in KEYMAP, passing the key and the function bound to it."
  (mapc
   (lambda (key-and-binding-cell)
     (cl-destructuring-bind (key . binding) key-and-binding-cell
       (funcall function key binding)))
   (keymap-to-key-binding-alist keymap)))

(defmacro variables-with-value (value)
  "Returns a list of names of variables that are `eq' to VALUE.
Doesn't see some variables!"
  (let ((return-value-var (make-symbol "return-value")))
    `(let ((,return-value-var (list)))
       (mapatoms (lambda (symbol)
                   (when (and (boundp symbol)
                              (eq (symbol-value symbol) ,value))
                     (push symbol ,return-value-var))))
       ,return-value-var)))

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
      (with-output-to-temp-buffer "*Keymaps with key*"
        (princ (concat "Keymaps with key " (key-description key) " bound in them:\n"))
        (mapc (lambda (keymap)
                (princ (concat" * `" (symbol-name keymap) "'\n")))
              keymaps)))
    keymaps))


(provide 'conf/utils/keys)
