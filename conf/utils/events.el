;;; Functions for events (single key/mouse combinations). -*- lexical-binding: t -*-

(defconst modifier-description-alist
  '((meta . "M-")
    (control . "C-")
    (shift . "S-")
    (hyper . "H-")
    (super . "s-")
    (alt . "A-"))
  "Alist of modifier symbols and their representations in `key-description'.")

(defun modifier-to-string (modifier)
  "Convert MODIFIER (e.g. `meta', see `modifier-description-alist') to a representation used in `kbd' (e.g. \"M-\")."
  (cdr (assq modifier modifier-description-alist)))

(defun event-to-string (event)
  "Convert EVENT to a string representation used in `kbd'."
  (key-description (vector event)))

(require 'conf/utils/strings) ; Used: string-replace-first.
(defun event-without-modifier (modifier event)
  "Return EVENT without MODIFIER (e.g. `meta', see `modifier-description-alist')."
  (let ((new-key-string
         (string-replace-first (modifier-to-string modifier) "" (event-to-string event))))
    (car (vector-to-list (kbd new-key-string)))))

(defun event-with-modifier (modifier event)
  "Return EVENT with MODIFIER (e.g. `meta', see `modifier-description-alist')."
  (let ((new-key-string
         (concat (modifier-to-string modifier) (event-to-string event))))
    (car (vector-to-list (kbd new-key-string)))))

(defun event-inverted-modifier (modifier event)
  "Return EVENT with the state of MODIFIER inverted (e.g. `meta', see `modifier-description-alist')."
  (if (memq modifier (event-modifiers event))
      (event-without-modifier modifier event)
    (event-with-modifier modifier event)))

(provide 'conf/utils/events)
