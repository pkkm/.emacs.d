;;; Keys for buffers.

(require 'conf/evil)

(defvar my-buffer-map (make-sparse-keymap)
  "Keymap for buffer-related commands.")
(define-prefix-command 'my-buffer-map)
(define-key evil-motion-state-map (kbd "-") 'my-buffer-map)

;; Switch.
(setq ido-ignore-buffers '("\\` " "\\`\\*"))
(define-key my-buffer-map (kbd "SPC") #'ido-switch-buffer)

;; Switch without ignoring any buffers.
(defun ido-switch-buffer-without-ignored ()
  "Like `ido-switch-buffer', but doesn't ignore any buffers."
  (interactive)
  (let ((ido-ignore-buffers '()))
    (ido-switch-buffer)))
(define-key my-buffer-map (kbd "b") #'ido-switch-buffer-without-ignored)

;; IBuffer -- advanced buffer switcher (distributed with Emacs).
(define-key my-buffer-map (kbd "C-SPC") #'ibuffer)
(define-key my-buffer-map (kbd "M-SPC") #'ibuffer)
(define-key my-buffer-map (kbd "C-b") #'ibuffer)

;; Switch to the most recently used buffer.
(define-key my-buffer-map (kbd "-") (lambda ()
                                      (interactive)
                                      (switch-to-buffer (other-buffer))))

;; Previous/next.
(define-key my-buffer-map (kbd "h") #'previous-buffer)
(define-key my-buffer-map (kbd "s") #'next-buffer)

;; Open in a split.
(define-key my-buffer-map (kbd "D") #'evil-split-next-buffer)
(define-key my-buffer-map (kbd "_") #'evil-split-next-buffer)

;; Open all buffers in a window.
(define-key my-buffer-map (kbd "a") #'display-all-buffers-in-windows)
(require 'conf/utils/buffers) ; Used: buffers-opened-in-windows.
(defun display-all-file-buffers-in-windows ()
  "Display all buffers that are visiting a file in windows."
  (interactive)
  (let (buffers-in-windows (buffers-opened-in-windows))
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer) ; Buffer is visiting a file.
                 (not (memq buffer buffers-in-windows))) ; FIXME: should prevent buffers that are already open from being opened again.
        (set-window-buffer (split-window (get-largest-window)) buffer)))
    (balance-windows)))

;; Delete (close/kill).
(define-key my-buffer-map (kbd "c") #'evil-delete-buffer) ; Delete buffer and window.
(define-key my-buffer-map (kbd "C") #'kill-this-buffer) ; Only delete the buffer.
(define-key my-buffer-map (kbd "k") #'ido-kill-buffer)

(provide 'conf/view/buffer-map)
