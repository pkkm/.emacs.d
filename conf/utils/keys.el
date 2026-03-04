;;; Utilities for key sequences and keymaps. -*- lexical-binding: t -*-

(defun clear-keymap (keymap)
  "Delete all bindings in KEYMAP.
The intuitive solution, (setq keymap-name (make-sparse-keymap)), doesn't work because Emacs accesses the keymap by pointer instead of name."
  (setcdr keymap nil))

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
