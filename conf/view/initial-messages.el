;;; Messages that are displayed when Emacs starts up. -*- lexical-binding: t -*-

;; Suppress the warning about .emacs.d being in `load-path'.
;; TODO: Is this still needed?
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
  (format "Init time: %.2f seconds."
          ;; Similar to `emacs-init-time'.
          (float-time (time-subtract after-init-time before-init-time))))

;; Don't display the startup screen.
(setq inhibit-startup-message t)

(provide 'conf/view/initial-messages)
