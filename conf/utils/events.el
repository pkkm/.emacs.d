;;; Functions for events (single key/mouse combinations).

(defconst modifier-description-alist
  '((meta . "M-")
    (control . "C-")
    (shift . "S-")
    (hyper . "H-")
    (super . "s-")
    (alt . "A-"))
  "Association list of modifier symbols and their representations in `key-description'.") ; TODO better name than "association list"?

(require 'conf/utils/strings) ; Used: string-replace-first-regexp-match.
(defun event-without-modifier (modifier event)
  "Return EVENT without the modifier key MODIFIER.
Modifier can be one of: `meta', `control', `shift', `hyper', `super', `alt'."
  (let ((new-key-description
         (string-replace-first-regexp-match (cdr (assq modifier modifier-description-alist)) ""
                                            (key-description (vector event)))))
    (car (vector-to-list (kbd new-key-description)))))

(defun event-with-modifier (modifier event)
  "Return EVENT with the modifier key MODIFIER.
Modifier can be one of: `meta', `control', `shift', `hyper', `super', `alt'."
  (let ((new-key-description (concat (cdr (assq modifier modifier-description-alist))
                                     (key-description (vector event)))))
    (car (vector-to-list (kbd new-key-description)))))

(defun event-inverted-modifier (modifier event)
  "Return EVENT with the state of the modifier key MODIFIER inverted.
Modifier can be one of: `meta', `control', `shift', `hyper', `super', `alt'."
  (if (memq modifier (event-modifiers event))
      (event-without-modifier modifier event)
    (event-with-modifier modifier event)))

(provide 'conf/utils/events)
