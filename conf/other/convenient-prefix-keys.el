;;; Bind keys from some prefixes in different ones, making them more convenient.

;; Conventions for the C-c prefix:
;;   * Letter (upper or lower case): reserved for the user.
;;   * Control character, digit, :, ;, {, }, <, >: major mode.
;;   * Other punctuation: minor mode (or sometimes major mode).

(with-eval-after-load 'evil
  (require 'conf/utils/keys) ; Used: map-bindings-between-keymaps.
  (require 'conf/utils/lists) ; Used: list-to-vector.
  (require 'conf/utils/events) ; Used: event-with-modifier, event-without-modifier, event-inverted-modifier.
  (require 'cl-lib) ; Used: cl-set-difference.

  (defun my-make-key-more-convenient (key)
    "Make the key vector KEY more convenient to hit after a prefix starting with a non-modified keypress.
Do that by inverting the state of Control on some events (all C-letter except C-h (help), C-m (RET), C-i (TAB))."
    (let ((first-event-of-key (first (vector-to-list key)))
          ;; Invert Control on all C-letters except C-h (help), C-m (RET), C-i (TAB).
          (control-inverted-events (cl-set-difference (number-sequence ?a ?z)
                                                      '(?h ?m ?i))))
      (cond
       ;; If KEY starts with C-`char' or `char', with `char' in `control-inverted-events',
       ;; invert Control state on all events in KEY that are in `control-inverted-events'.
       ((or (memq first-event-of-key control-inverted-events)
            (memq (event-inverted-modifier 'control first-event-of-key) control-inverted-events))
        (list-to-vector
         (mapcar (lambda (event)
                   (if (or (memq event control-inverted-events)
                           (memq (event-inverted-modifier 'control event) control-inverted-events))
                       (event-inverted-modifier 'control event)
                     event))
                 key)))
       ;; Otherwise, don't modify KEY.
       (t
        key))))

  ;; Make "e" a prefix for "C-x" commands, and "C-e" the former "e" (end of word).
  (defun rebind-C-x-to-e ()
    "Bind all \"C-x\" bindings in the \"e\" prefix (in `evil-motion-state-map'), with the `my-make-key-more-convenient' transformation."
    (interactive)
    (bind-key "e" nil evil-motion-state-map)
    (map-key-sequences-in-keymap (key-binding (kbd "C-x"))
                                 (lambda (key binding)
                                   (define-key evil-motion-state-map
                                     (concat-keys (kbd "e") (my-make-key-more-convenient key))
                                     binding))))
  (bind-key "C-e" #'evil-forward-word-end evil-motion-state-map)
  (add-hook 'emacs-startup-hook #'rebind-C-x-to-e)

  ;; Make "SPC" a prefix for "C-c" commands.
  (defun rebind-C-c-to-SPC ()
    "Bind all \"C-c\" bindings in the \"SPC\" prefix (in the current major mode's map), with the `my-make-key-more-convenient' transformation.
Omit \"C-c [a-zA-Z]\" bindings, since they are not major-mode bindings, but user's custom ones."
    (interactive)
    (bind-key "SPC" nil evil-motion-state-local-map)
    (map-key-sequences-in-keymap (key-binding (kbd "C-c"))
                                 (lambda (key binding)
                                   ;; Ignore the binding if KEY starts with a character from [a-zA-Z].
                                   (unless (memq (first (vector-to-list key))
                                                 (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
                                     (define-key evil-motion-state-local-map
                                       (concat-keys (kbd "SPC") (my-make-key-more-convenient key))
                                       binding)))))
  (bind-key "SPC" nil evil-motion-state-map)
  (add-hook 'after-change-major-mode-hook #'rebind-C-c-to-SPC t)
  (add-hook 'emacs-startup-hook #'rebind-C-c-to-SPC))

(provide 'conf/other/convenient-prefix-keys)
