;;; Messages that are displayed when Emacs starts up. -*- lexical-binding: t -*-

;; Suppress the warning about .emacs.d being in `load-path'.
(defun my-suppress-load-path-warning (orig-fun type message &rest args)
  "Silence the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    (apply orig-fun type message args)))
(advice-add 'display-warning :around #'my-suppress-load-path-warning)

;; Start with an empty *scratch* buffer.
(setq initial-scratch-message nil)

;; Show init time in the minibuffer instead of the default message.
(defun startup-echo-area-message ()
  (emacs-init-time "Init time: %.2f seconds."))

;; Don't display the startup screen.
(setq inhibit-startup-screen t)

(provide 'conf/view/initial-messages)
