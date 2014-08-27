;;; Messages that are displayed when Emacs starts up.

;; Don't display warnings of type "initialization".
;; This suppresses the warning on every Emacs startup that .emacs.d is in the `load-path'.
(dolist (var '(warning-suppress-types warning-suppress-log-types))
  (if (boundp var)
      (add-to-list var '(initialization))
    (set var '((initialization)))))

;; Start with an empty *scratch* buffer.
(setq initial-scratch-message nil)

;; Show init time in the minibuffer instead of the default message.
(defun startup-echo-area-message ()
  (format "Init time: %s." (emacs-init-time)))

;; Don't display the startup screen.
(setq inhibit-startup-message t)

(provide 'conf/view/initial-messages)
