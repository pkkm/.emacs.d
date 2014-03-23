;;; Windows.

(require 'conf/evil)

;; Track the most recently created window.
(defvar most-recently-created-window nil
  "The window that was most recently created. This variable gets set when `split-window-internal' is called.")
(defadvice split-window-internal (after record-window activate)
  (setq most-recently-created-window ad-return-value))

(defun delete-most-recently-created-window ()
  "Delete the window that was most recently created."
  (interactive)
  (delete-window most-recently-created-window))
(defun kill-most-recently-created-window-and-buffer ()
  "Delete the window that was most recently created and kill the buffer that it was displaying."
  (interactive)
  (select-window most-recently-created-window)
  (kill-buffer-and-window))

(global-set-key (kbd "<f5>") #'show-most-recently-created-window)

(defvar my-window-map (make-sparse-keymap)
  "Keymap for my window-related commands.")
(define-prefix-command 'my-window-map)
(define-key evil-motion-state-map (kbd "<tab>") 'my-window-map)
(define-key evil-motion-state-map (kbd "TAB") 'my-window-map)


;;; Frequently used.

;; Split (with count: leave COUNT lines in the initially-selected window).
(define-key my-window-map (kbd "d") #'split-window-horizontally)
(define-key my-window-map (kbd "-") #'split-window-vertically)

;; Close.
(define-key my-window-map (kbd "o") #'delete-other-windows)
(define-key my-window-map (kbd "c") #'delete-window)
(define-key my-window-map (kbd "C") #'delete-most-recently-created-window)
(define-key my-window-map (kbd "k") #'evil-delete-buffer) ; Kill buffer and window.
(define-key my-window-map (kbd "K") #'kill-most-recently-created-window-and-buffer)

;; Move focus.
(define-key my-window-map (kbd "h") #'evil-window-left)
(define-key my-window-map (kbd "t") #'evil-window-down)
(define-key my-window-map (kbd "n") #'evil-window-up)
(define-key my-window-map (kbd "s") #'evil-window-right)

;; Next, previous.
(define-key my-window-map (kbd "TAB") #'evil-window-next)
(define-key my-window-map (kbd "<backtab>") #'evil-window-prev) ; <backtab> -- S-TAB.

;; Select the window with the most recently used buffer.
(define-key my-window-map (kbd "SPC") #'evil-window-mru)

;; Move.
(define-key my-window-map (kbd "H") #'evil-window-move-far-left)
(define-key my-window-map (kbd "T") #'evil-window-move-very-bottom)
(define-key my-window-map (kbd "N") #'evil-window-move-very-top)
(define-key my-window-map (kbd "S") #'evil-window-move-far-right)

;; Resize (with count: by COUNT columns/lines).
(define-key my-window-map (kbd "C-h") #'evil-window-decrease-width)
(define-key my-window-map (kbd "C-t") #'evil-window-increase-height)
(define-key my-window-map (kbd "C-n") #'evil-window-decrease-height)
(define-key my-window-map (kbd "C-s") #'evil-window-increase-width)

;; Make windows equal in size.
(define-key my-window-map (kbd "=") #'balance-windows)

;; Rotate (first -> second, ..., last -> first).
(define-key my-window-map (kbd "r") #'evil-window-rotate-downwards)
(define-key my-window-map (kbd "R") #'evil-window-rotate-upwards)

;; Undo (restore previous window configuration).
(winner-mode 1) ; winner-mode is included with Emacs.
(setq winner-mode-map (make-sparse-keymap)) ; Disable winner-mode's default bindings.
(define-key my-window-map (kbd "u") #'winner-undo)
(define-key my-window-map (kbd "C-r") #'winner-redo)


;;; Rarely unused.

;; Split and open new file (with count: leave COUNT lines in the initially-selected window).
(define-key my-window-map (kbd "D") #'evil-window-vnew) ; Horizontally.
(define-key my-window-map (kbd "_") #'evil-window-new) ; Vertically.

;; Set width/height to COUNT.
(define-key my-window-map (kbd "M-t") #'evil-window-set-height)
(define-key my-window-map (kbd "M-s") #'evil-window-set-width)

;; First (top-left), last (bottom-right).
(define-key my-window-map (kbd "f") #'evil-window-top-left)
(define-key my-window-map (kbd "l") #'evil-window-bottom-right)


(provide 'conf/view/windows)
