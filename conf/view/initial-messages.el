;;; Messages that are displayed when Emacs starts up. -*- lexical-binding: t -*-

;; Suppress the warning about .emacs.d being in `load-path'.
(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    ad-do-it))

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
