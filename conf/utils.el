;;; Misc. utilities.

(defun event-with-modifier (modifier event)
  (let ((mods (event-modifiers event))
        (base (event-basic-type event)))
    (event-convert-list (append (cons modifier mods) (list base)))))

(defun event-without-modifier (modifier event)
  (let* ((mods (event-modifiers event))
         (base (event-basic-type event))
         (new-mods (seq-remove (lambda (m) (eq m modifier)) mods)))
    (event-convert-list (append new-mods (list base)))))

(defun event-toggle-modifier (modifier event)
  (let* ((mods (event-modifiers event))
         (base (event-basic-type event))
         (new-mods (if (seq-contains-p mods modifier #'eq)
                       (seq-remove (lambda (m) (eq m modifier)) mods)
                     (cons modifier mods))))
    (event-convert-list (append new-mods (list base)))))

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

(defun string-replace-first (old new string)
  "Replace the first occurrence of OLD with NEW in STRING."
  (if (string-match (regexp-quote old) string)
      (replace-match new t t string)
    string))

(defun join-nonempty (separator &rest parts)
  "Return a string with nonempty (not nil or \"\") elements of PARTS joined with SEPARATOR."
  (declare (indent defun))
  (string-join (delq nil (delete "" parts)) separator))

(defun add-to-list-after (list-var element after-what)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
ELEMENT will be added after the first occurrence of AFTER-WHAT,
or at the beginning if AFTER-WHAT isn't in the list. Comparisons
are done with `equal'."
  (let ((lst (symbol-value list-var)))
    (unless (member element lst)
      (if-let ((tail (member after-what lst)))
          (setcdr tail (cons element (cdr tail)))
        (set list-var (cons element lst))))))

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

(require 'color) ; Used: color-rgb-to-hex, color-name-to-rgb, color-defined-p.
(require 'cl-lib) ; Used: cl-destructuring-bind.

(defun my-color-mix (&rest colors-weights-list)
  "Mix colors in the given proportions.
Example: (my-color-mix \"#ffffff\" 0.6 \"#ff0000\" 0.4) => \"#ff9999\".
If the sum of weights (proportions) is larger than 1.0, the result will be brighter.
If one of the colors is unspecified (cannot be displayed), return 'unspecified."
  (let* ((color-weight-pairs (seq-partition colors-weights-list 2)) ; '((color weight) ...)
         (all-colors-defined-p (seq-every-p (lambda (color-weight-pair)
                                              (color-defined-p (car color-weight-pair)))
                                            color-weight-pairs)))
    (if all-colors-defined-p
        (thread-last
          color-weight-pairs
          ;; Change pairs of (color-name weight) to triples of (red green blue).
          (mapcar (lambda (color-weight-pair)
                    (cl-destructuring-bind (color-name weight) color-weight-pair
                      (let ((color-rgb (color-name-to-rgb color-name)))
                        (mapcar (lambda (rgb-component) (* rgb-component weight))
                                color-rgb)))))
          ;; Sum the list of (red green blue) triples.
          (-reduce (lambda (rgb-1 rgb-2)
                     (cl-mapcar '+ rgb-1 rgb-2)))
          ;; Convert to hex notation.
          (apply #'color-rgb-to-hex))
      'unspecified)))

(defun my-buffers-visible-in-windows (&optional all-frames)
  "Return a list of all buffers currently displayed in windows, without duplicates."
  (seq-uniq (mapcar #'window-buffer (window-list-1 nil 'no-minibuffer t))))

(defmacro define-interactive-wrapper (name arglist copy-from &optional docstring &rest body)
  "Define a function that uses the `interactive' spec from another function.
For creating future-proof passthroughs."
  (declare (doc-string 4) (indent 3))
  `(defun ,name ,arglist
     ,@(when (stringp docstring) (list docstring))
     ;; See <https://emacs.stackexchange.com/a/19242>.
     (interactive (advice-eval-interactive-spec
                   (cadr (interactive-form #',copy-from))))
     ;; Replace `(diw-apply-original-fun args)' with an application of COPY-FROM that has the right interactivity.
     ,@(-tree-map-nodes
        (lambda (node) (and (consp node) (eq (car node) 'diw-apply-original-fun)))
        (lambda (node)
          `(apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
                  #',copy-from ,@(cdr node)))
        (if (stringp docstring) body (cons docstring body)))))

(defun test-https-verification ()
  "Test Emacs' HTTPS verification by contacting a few sites. Signal an error if an invalid certificate is accepted."
  (interactive)
  (require 'url)
  (dolist (good-url '("https://github.com/"
                      "https://facebook.com/"))
    (kill-buffer (url-retrieve-synchronously good-url)))
  (dolist (bad-url '("https://self-signed.badssl.com/"
                     "https://wrong.host.badssl.com/"
                     "https://untrusted-root.badssl.com/"
                     "https://revoked.grc.com/"
                     "https://dh480.badssl.com/"
                     "https://rc4.badssl.com/"))
    (when (condition-case nil
              (kill-buffer (url-retrieve-synchronously bad-url))
            (error nil))
      (error "Bad HTTPS verification: no error on site with bad cert: %s" bad-url)))
  (message "HTTPS verification OK"))

(provide 'conf/utils)
