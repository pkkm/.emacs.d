;;; Messages that are displayed when Emacs starts up.

;; Don't display warnings of type "initialization".
;; This suppresses the warning on every Emacs startup that .emacs.d is in the `load-path'.
(dolist (var '(warning-suppress-types warning-suppress-log-types))
  (if (boundp var)
      (add-to-list var '(initialization))
    (set var '((initialization)))))

;; Display the init time in the *scratch* buffer.
(defun display-init-time-in-scratch-buffer ()
  (setq initial-scratch-message
        (format ";; Init time: %s.\n\n" (emacs-init-time))))
(add-hook 'after-init-hook #'display-init-time-in-scratch-buffer)

;; Don't display the startup screen.
(setq inhibit-startup-message t)

(provide 'conf/view/initial-messages)
