;;; Functions for events (single key/mouse combinations). -*- lexical-binding: t -*-

;; Modifier symbols in Emacs: `meta', `control', `shift', `super', `hyper', `alt'.

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

(provide 'conf/utils/events)
