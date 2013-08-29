;;; Replacements for `universal-argument' and `negative-argument' for use in Evil.

(require 'conf/evil)

(define-key evil-motion-state-map (kbd "N") #'evil-universal-argument)
(define-key evil-motion-state-map (kbd "_") #'evil-negative-argument)

(defvar evil-universal-argument-map (make-sparse-keymap)
  "Replacement for `universal-argument-map' to use when `universal-argument' is used as an Evil command.")
(define-key evil-universal-argument-map [?N] #'universal-argument-more)
(define-key evil-universal-argument-map [t] #'universal-argument-other-key)
(define-key evil-universal-argument-map (vector meta-prefix-char t) #'universal-argument-other-key)

(defun evil-universal-argument ()
  "Like `universal-argument', but expects to be bound to N instead of C-u and doesn't add special handling for following digits and \"-\"."
  (interactive)
  (let ((universal-argument-map evil-universal-argument-map))
    (call-interactively #'universal-argument)))

(defun evil-negative-argument ()
  "Like `negative-argument', but doesn't add special handling for following digits and \"-\"."
  (interactive)
  (let ((universal-argument-map evil-universal-argument-map))
    (call-interactively #'negative-argument)))

;; Declare the commands to be nonrepeatable.
(evil-declare-not-repeat 'evil-universal-argument)
(evil-declare-not-repeat 'evil-negative-argument)

(provide 'conf/operators/prefix-arguments)
