;;; Keys for buffers.

(defvar my-buffer-map (make-sparse-keymap)
  "Keymap for buffer-related commands.")
(define-prefix-command 'my-buffer-map)
(with-eval-after-load 'evil
  (bind-key "-" 'my-buffer-map evil-motion-state-map))

;; Switch.
;; Press C-k to kill the highlighted buffer.
(setq ido-ignore-buffers '("\\` " "\\`\\*"))
(bind-key "SPC" #'ido-switch-buffer my-buffer-map)

;; Switch without ignoring any buffers.
(defun ido-switch-buffer-without-ignored ()
  "Like `ido-switch-buffer', but doesn't ignore any buffers."
  (interactive)
  (let ((ido-ignore-buffers '()))
    (ido-switch-buffer)))
(bind-key "b" #'ido-switch-buffer-without-ignored my-buffer-map)

;; IBuffer -- advanced buffer switcher (distributed with Emacs).
;; Useful for doing operations on many buffers simultaneously.
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
;;   A/H -- view in this/another frame
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
(bind-key "C-b" #'ibuffer my-buffer-map)

;; Switch to the most recently used buffer.
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(bind-key "-" #'switch-to-other-buffer my-buffer-map)

;; Previous/next.
(bind-key "h" #'previous-buffer my-buffer-map)
(bind-key "s" #'next-buffer my-buffer-map)

;; Open all buffers in windows.
(bind-key "a" #'display-all-file-buffers-in-windows my-buffer-map)
(require 'conf/utils/buffers) ; Used: buffers-opened-in-windows.
(use-package dash :ensure dash :commands (--> -difference -filter -map))
(defun display-all-file-buffers-in-windows ()
  "Display all buffers that are visiting a file in windows."
  (interactive)
  (->> (-difference (buffer-list) (buffers-opened-in-windows))
    (-filter (lambda (buffer) (buffer-file-name buffer))) ; Only buffers that are visiting a file.
    (-map (lambda (buffer)
            (let ((new-window (funcall split-window-preferred-function (get-largest-window))))
              (when new-window ;; If the splitting was successful (there was enough space to sensibly split)...
                (set-window-buffer new-window buffer))))))
  (balance-windows))

;; Delete (close/kill).
(bind-key "c" #'kill-this-buffer my-buffer-map) ; Only delete the buffer.

(provide 'conf/view/buffers/buffer-bindings)
