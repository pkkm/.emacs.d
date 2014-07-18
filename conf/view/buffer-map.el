;;; Keys for buffers.
;; When selecting a buffer using Ido (- SPC or - b), press C-k to kill the highlighted buffer.

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
(define-key my-buffer-map (kbd "C-b") #'ibuffer)

;; Switch to the most recently used buffer.
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key my-buffer-map (kbd "-") #'switch-to-other-buffer)

;; Previous/next.
(define-key my-buffer-map (kbd "h") #'previous-buffer)
(define-key my-buffer-map (kbd "s") #'next-buffer)

;; Open all buffers in windows.
(define-key my-buffer-map (kbd "a") #'display-all-file-buffers-in-windows)
(require 'conf/utils/buffers) ; Used: buffers-opened-in-windows.
(require 'conf/packages) (package-ensure-installed 'dash) (require 'dash) ; Used: -->, -difference, -filter, -map.
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
(define-key my-buffer-map (kbd "c") #'kill-this-buffer) ; Only delete the buffer.

(provide 'conf/view/buffer-map)
