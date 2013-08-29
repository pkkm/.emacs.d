;;; Bind keys after the "C-x" prefix to more convenient ones after the "e" prefix.

;; Key sequences starting with "C-c":
;;   * Letter (upper or lower case): reserved for the user.
;;   * Control character, digit, :, ;, {, }, <, >: major mode.
;;   * Other punctuation: minor mode (or sometimes major mode).

;; Parts of config that define C-x keys.
(require 'conf/minibuffer/M-x)

(require 'conf/evil)
(require 'conf/utils/keys) ; Used: map-bindings-between-keymaps.
(require 'conf/utils/lists) ; Used: list-to-vector.
(require 'conf/utils/events) ; Used: event-with-modifier, event-without-modifier.
(require 'cl-lib)

(defun rebind-C-x-to-e ()
  (map-bindings-between-keymaps
   (key-binding (kbd "C-x"))
   evil-motion-state-map
   (lambda (key)
     (concat-keys (kbd "e") (my-make-key-more-convenient key)))))

(defun my-make-key-more-convenient (key)
  "Make the key vector KEY more convenient to hit after a prefix starting with a non-modified keypress.
For example, change some control-keypresses to non-modified keypresses and the other way around."
  (let* ((first-event-of-key (first (vector-to-list key)))
         (not-control-inverted-letters '(?h ?m ?i)) ; Don't invert Control on: C-h, C-m (RET), C-i (TAB).
         (control-inverted-letters (cl-set-difference (number-sequence ?a ?z)
                                                      not-control-inverted-letters)))
    (cond
     ;; If KEY starts with C-letter, where letter is in `control-inverted-letters',
     ;; invert Control state on all letters in KEY that are in `control-inverted-letters'.
     ((or (memq first-event-of-key control-inverted-letters)
          (memq (event-without-modifier 'control first-event-of-key) control-inverted-letters))
      (list-to-vector
       (mapcar (lambda (event)
                 (cond
                  ((memq event control-inverted-letters)
                   (event-with-modifier 'control event))
                  ((memq (event-without-modifier 'control event) control-inverted-letters)
                   (event-without-modifier 'control event))
                  (t
                   event)))
               key)))
     ;; Otherwise, don't modify KEY.
     (t
      key))))

;; Make "e" a prefix for C-x commands, and "C-e" the former "e" (end of word).
(define-key evil-motion-state-map (kbd "C-e") 'evil-forward-word-end)
(define-key evil-motion-state-map (kbd "e") 'nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (rebind-C-x-to-e)))

(provide 'conf/other/make-prefix-keys-more-convenient)
