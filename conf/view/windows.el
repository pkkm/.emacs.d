;;; Windows.

(require 'conf/evil)

;; Track the most recently created window.
(defvar most-recently-created-window nil
  "The window that was most recently created. This variable gets set when `split-window-internal' is called.")
(defadvice split-window-internal (after record-window activate)
  (setq most-recently-created-window ad-return-value))

;; Commands to delete the most recently created window.
(defun delete-most-recently-created-window ()
  "Delete the window that was most recently created."
  (interactive)
  (delete-window most-recently-created-window))
(defun kill-most-recently-created-window-and-buffer ()
  "Delete the window that was most recently created and kill the buffer that it was displaying."
  (interactive)
  (select-window most-recently-created-window)
  (kill-buffer-and-window))

;; Split window, focus it and display the next buffer in it.
(defun my-split-window-right ()
  (interactive)
  (select-window (call-interactively #'split-window-right))
  (switch-to-buffer (other-buffer)))
(defun my-split-window-below ()
  (interactive)
  (select-window (call-interactively #'split-window-below))
  (switch-to-buffer (other-buffer)))

(defvar my-window-map (make-sparse-keymap)
  "Keymap for my window-related commands.")
(define-prefix-command 'my-window-map)
(define-key evil-motion-state-map (kbd "<tab>") 'my-window-map)
(define-key evil-motion-state-map (kbd "TAB") 'my-window-map)


;; Split (with count: leave COUNT lines in the initially-selected window).
(define-key my-window-map (kbd "d") #'my-split-window-right)
(define-key my-window-map (kbd "-") #'my-split-window-below)

;; Close.
(define-key my-window-map (kbd "o") #'delete-other-windows)
(define-key my-window-map (kbd "c") #'delete-window)
(define-key my-window-map (kbd "k") #'evil-delete-buffer) ; Kill buffer and window.
(define-key my-window-map (kbd "C") #'delete-most-recently-created-window)
(define-key my-window-map (kbd "K") #'kill-most-recently-created-window-and-buffer)

;; Next, previous.
(define-key my-window-map (kbd "TAB") #'evil-window-next)
(define-key my-window-map (kbd "<backtab>") #'evil-window-prev) ; <backtab> -- S-TAB.

;; Select the window with the most recently used buffer.
(define-key my-window-map (kbd "SPC") #'evil-window-mru)

;; Move focus directionally.
(define-key my-window-map (kbd "h") #'evil-window-left)
(define-key my-window-map (kbd "t") #'evil-window-down)
(define-key my-window-map (kbd "n") #'evil-window-up)
(define-key my-window-map (kbd "s") #'evil-window-right)

;; Move window.
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

;; Ace-jump for windows.
(require 'conf/packages)
(package-ensure-installed 'ace-window)
(define-key my-window-map (kbd "RET") #'ace-window)
(setq aw-keys (string-to-list "htnsdueoaigcrlfp.,;y"))


(provide 'conf/view/windows)
