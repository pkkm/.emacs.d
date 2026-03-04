;;; Keys for buffers. -*- lexical-binding: t -*-

(defvar-keymap my-buffer-map
  :doc "Keymap for buffer-related commands."
  :prefix 'my-buffer-map)
(with-eval-after-load 'evil
  (evil-define-key 'motion 'global (kbd "-") 'my-buffer-map))

;; Switch.
;; C-k -- kill the highlighted buffer.
;; C-a -- toggle showing all buffers.
(bind-key "SPC" #'ido-switch-buffer my-buffer-map)

;; Switch to the most recently used buffer.
(defun my-switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(bind-key "-" #'my-switch-to-other-buffer my-buffer-map)

;; Delete (close/kill).
(bind-key "c" #'kill-current-buffer my-buffer-map)

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

;; Display all buffers that are visiting a file.
(bind-key "a" #'my-display-all-file-buffers my-buffer-map)
(require 'conf/utils) ; Used: my-buffers-visible-in-windows.
(defun my-display-all-file-buffers ()
  "Display all buffers that are visiting a file."
  (interactive)
  (let* ((visible-buffers (mapcar #'window-buffer (window-list)))
         (buffers-to-show (seq-filter (lambda (buf)
                                        (and (buffer-file-name buf)
                                             (not (memq buf visible-buffers))))
                                      (buffer-list))))
    (dolist (buf buffers-to-show)
      (display-buffer buf '((display-buffer-pop-up-window)
                            (inhibit-same-window . t))))
    (balance-windows)))

(provide 'conf/view/buffer-bindings)
