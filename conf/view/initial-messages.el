;;; Messages that are displayed when Emacs starts up.

;; Don't display warnings of type "initialization".
;; This suppresses the warning on every Emacs startup that .emacs.d is in the `load-path'.
(dolist (var '(warning-suppress-types warning-suppress-log-types))
  (if (boundp var)
      (add-to-list var '(initialization))
    (set var '((initialization)))))

;; Make the *scratch* buffer empty.
(setq initial-scratch-message nil)

;; Don't display the startup screen.
(setq inhibit-startup-message t)

(provide 'conf/view/initial-messages)
