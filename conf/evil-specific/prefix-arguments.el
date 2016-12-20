;;; Replacements for `universal-argument' and `negative-argument' for use in Evil. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  ;; Make C-u in insert state kill backward to indentation.
  (bind-key "C-u" #'kill-back-to-indentation evil-insert-state-map)
  (defun kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line.
If point is between the beginning of line and the first non-whitespace character, kill to beginning of line instead."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (when (>= (point) prev-pos)
        (beginning-of-line))
      (kill-region (point) prev-pos)))

  ;; Start an universal argument with U, and a negative argument with _.
  (bind-key "U" #'evil-universal-argument evil-motion-state-map)
  (bind-key "_" #'evil-negative-argument evil-motion-state-map)

  (defvar evil-universal-argument-map (make-sparse-keymap)
    "Replacement for `universal-argument-map' to use with `evil-universal-argument' and `evil-negative-argument'.")
  (bind-key "U" #'universal-argument-more evil-universal-argument-map)

  (defun evil-universal-argument ()
    "Like `universal-argument', but expects to be bound to U instead of C-u and doesn't add special handling for digits and \"-\"."
    (interactive)
    (let ((universal-argument-map evil-universal-argument-map))
      (call-interactively #'universal-argument)))

  (defun evil-negative-argument ()
    "Like `negative-argument', but doesn't add special handling for digits and \"-\"."
    (interactive)
    (let ((universal-argument-map evil-universal-argument-map))
      (call-interactively #'negative-argument)))

  ;; Declare the commands to be nonrepeatable.
  (evil-declare-not-repeat 'evil-universal-argument)
  (evil-declare-not-repeat 'evil-negative-argument))

(provide 'conf/evil-specific/prefix-arguments)
