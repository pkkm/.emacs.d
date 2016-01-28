;;; Keys for buffers.

(defvar my-buffer-map (make-sparse-keymap)
  "Keymap for buffer-related commands.")
(define-prefix-command 'my-buffer-map)
(with-eval-after-load 'evil
  (bind-key "-" 'my-buffer-map evil-motion-state-map))

;; Switch.
;; C-k -- kill the highlighted buffer.
;; C-a -- toggle showing all buffers.
(bind-key "SPC" #'ido-switch-buffer my-buffer-map)

;; Switch to the most recently used buffer.
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(bind-key "-" #'switch-to-other-buffer my-buffer-map)

;; Previous/next.
(bind-key "h" #'previous-buffer my-buffer-map)
(bind-key "l" #'next-buffer my-buffer-map)

;; Delete (close/kill).
(bind-key "c" #'kill-this-buffer my-buffer-map)

;; IBuffer -- advanced buffer switcher (bundled with Emacs). Useful for batch operations.
;; Marking:
;;   m -- mark buffer at point
;;   u -- unmark buffer at point
;;   M-<backspace> -- unmark all
;;   t -- invert marks
;; Marking by predicate:
;;   * M -- by major mode
;;   * u -- unsaved (modified and visiting a file)
;;   * s -- name begins and ends with *
;;   . -- older than `ibuffer-old-time'
;; Operations on marked buffers:
;;   S -- save
;;   D -- kill
;;   A -- view
;;   U -- replace by regexp
;;   Q/I -- query-replace/query-replace-regexp
;;   ! -- run a shell command on each buffer with its file name as the argument
;;   E -- eval in each buffer
;; Other:
;;   RET -- view buffer at point
;;   o -- view buffer at point in other window
;;   g -- refresh
;;   h -- help
;;   q -- quit
(bind-key "b" #'ibuffer my-buffer-map)

(provide 'conf/view/buffer-bindings)
