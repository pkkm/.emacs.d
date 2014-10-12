;;; Windows.

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
(with-eval-after-load 'evil
  (bind-key "<tab>" 'my-window-map evil-motion-state-map)
  (bind-key "TAB" 'my-window-map evil-motion-state-map))


;; Split (with count: leave COUNT lines in the initially-selected window).
(bind-key "d" #'my-split-window-right my-window-map)
(bind-key "-" #'my-split-window-below my-window-map)

;; Close.
(bind-key "o" #'delete-other-windows my-window-map)
(bind-key "c" #'delete-window my-window-map)
(bind-key "k" #'evil-delete-buffer my-window-map) ; Kill buffer and window.
(bind-key "C" #'delete-most-recently-created-window my-window-map)
(bind-key "K" #'kill-most-recently-created-window-and-buffer my-window-map)

;; Next, previous.
(bind-key "TAB" #'evil-window-next my-window-map)
(bind-key "<backtab>" #'evil-window-prev my-window-map) ; <backtab> -- S-TAB.

;; Select the window with the most recently used buffer.
(bind-key "SPC" #'evil-window-mru my-window-map)

;; Move focus directionally.
(bind-key "h" #'evil-window-left my-window-map)
(bind-key "t" #'evil-window-down my-window-map)
(bind-key "n" #'evil-window-up my-window-map)
(bind-key "s" #'evil-window-right my-window-map)

;; Move window.
(bind-key "H" #'evil-window-move-far-left my-window-map)
(bind-key "T" #'evil-window-move-very-bottom my-window-map)
(bind-key "N" #'evil-window-move-very-top my-window-map)
(bind-key "S" #'evil-window-move-far-right my-window-map)

;; Resize (with count: by COUNT columns/lines).
(bind-key "C-h" #'evil-window-decrease-width my-window-map)
(bind-key "C-t" #'evil-window-increase-height my-window-map)
(bind-key "C-n" #'evil-window-decrease-height my-window-map)
(bind-key "C-s" #'evil-window-increase-width my-window-map)

;; Make windows equal in size.
(bind-key "=" #'balance-windows my-window-map)

;; Rotate (first -> second, ..., last -> first).
(bind-key "r" #'evil-window-rotate-downwards my-window-map)
(bind-key "R" #'evil-window-rotate-upwards my-window-map)

;; Undo (restore previous window configuration).
(use-package winner ; Bundled with Emacs.
  :demand t
  :pre-load
  (setq winner-dont-bind-my-keys t) ; Don't bind C-c <left> or C-c <right>.
  :init
  (winner-mode 1)
  (bind-key "u" #'winner-undo my-window-map)
  (bind-key "C-r" #'winner-redo my-window-map))

;; Save/restore window configuration from register.
(bind-key "m" #'window-configuration-to-register my-window-map)
(bind-key "'" #'jump-to-register my-window-map)


(provide 'conf/view/windows/window-bindings)
