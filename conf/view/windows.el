;;; Keys for managing windows.

(require 'conf/evil)

(defvar my-window-map (make-sparse-keymap)
  "Keymap for my window-related commands.")
(define-prefix-command 'my-window-map)
(define-key evil-motion-state-map (kbd "<tab>") 'my-window-map)
(define-key evil-motion-state-map (kbd "TAB") 'my-window-map)

;; Split (with count: leave COUNT lines in the initially-selected window).
(define-key my-window-map (kbd "d") 'split-window-horizontally)
(define-key my-window-map (kbd "-") 'split-window-vertically)

;; Split and open new file (with count: leave COUNT lines in the initially-selected window).
(define-key my-window-map (kbd "D") 'evil-window-vnew) ; Horizontally.
(define-key my-window-map (kbd "_") 'evil-window-new) ; Vertically.

;; Close.
(define-key my-window-map (kbd "c") 'delete-window)
(define-key my-window-map (kbd "o") 'delete-other-windows)

;; Move focus.
(define-key my-window-map (kbd "h") 'evil-window-left)
(define-key my-window-map (kbd "t") 'evil-window-down)
(define-key my-window-map (kbd "n") 'evil-window-up)
(define-key my-window-map (kbd "s") 'evil-window-right)

;; Next, previous.
;; TODO replace with next-window/previous-window?
(define-key my-window-map (kbd "TAB") 'evil-window-next)
(define-key my-window-map (kbd "<backtab>") 'evil-window-prev) ; <backtab> -- S-TAB.
(define-key my-window-map (kbd "M-TAB") 'evil-window-prev)

;; First (top-left), last (bottom-right).
(define-key my-window-map (kbd "f") 'evil-window-top-left)
(define-key my-window-map (kbd "l") 'evil-window-bottom-right)

;; Select the window with the most recently used buffer.
(define-key my-window-map (kbd "p") 'evil-window-mru)
(define-key my-window-map (kbd "SPC") 'evil-window-mru)

;; Move.
(define-key my-window-map (kbd "H") 'evil-window-move-far-left)
(define-key my-window-map (kbd "T") 'evil-window-move-very-bottom)
(define-key my-window-map (kbd "N") 'evil-window-move-very-top)
(define-key my-window-map (kbd "S") 'evil-window-move-far-right)

;; Rotate (first -> second, ..., last -> first).
(define-key my-window-map (kbd "r") 'evil-window-rotate-downwards)
(define-key my-window-map (kbd "R") 'evil-window-rotate-upwards)

;; Resize (with count: by COUNT columns/lines).
(define-key my-window-map (kbd "C-h") 'evil-window-decrease-width)
(define-key my-window-map (kbd "C-t") 'evil-window-increase-height)
(define-key my-window-map (kbd "C-n") 'evil-window-decrease-height)
(define-key my-window-map (kbd "C-s") 'evil-window-increase-width)

;; Set width/height to COUNT.
(define-key my-window-map (kbd "M-t") 'evil-window-set-height)
(define-key my-window-map (kbd "M-s") 'evil-window-set-width)

;; Make windows equal in size.
(define-key my-window-map (kbd "=") 'balance-windows)

(provide 'conf/view/windows)
