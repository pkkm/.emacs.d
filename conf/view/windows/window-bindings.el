;;; Windows.

;; Track the most recently created window.
(defvar most-recently-created-window nil
  "The window that was most recently created.")
(defadvice split-window-internal (after record-window activate)
  (setq most-recently-created-window ad-return-value))

(defun delete-most-recently-created-window ()
  "Delete the most recently created window."
  (interactive)
  (delete-window most-recently-created-window))

;; Split window, focus it and display the next buffer in it.
(defun my-split-window-right ()
  (interactive)
  (call-interactively #'evil-window-vsplit)
  (select-window most-recently-created-window)
  (switch-to-buffer (other-buffer)))
(defun my-split-window-below ()
  (interactive)
  (call-interactively #'evil-window-split)
  (select-window most-recently-created-window)
  (switch-to-buffer (other-buffer)))

(with-eval-after-load 'evil
  (bind-key "TAB" 'evil-window-map evil-motion-state-map)

  ;; Split (with count: leave COUNT lines in the initially-selected window).
  (bind-key "d" #'my-split-window-right evil-window-map)
  (bind-key "-" #'my-split-window-below evil-window-map)

  ;; Close.
  (bind-key "K" #'evil-delete-buffer evil-window-map) ; Kill buffer and window.
  (bind-key "C" #'delete-most-recently-created-window evil-window-map)

  ;; Next, previous.
  (bind-key "TAB" #'evil-window-next evil-window-map)
  (bind-key "<backtab>" #'evil-window-prev evil-window-map) ; <backtab> -- S-TAB.

  ;; Select the window with the most recently used buffer.
  (bind-key "SPC" #'evil-window-mru evil-window-map)

  ;; Navigate/move with hjks instead of hjkl.
  (bind-key "s" #'evil-window-right evil-window-map)
  (bind-key "S" #'evil-window-move-far-right evil-window-map)

  ;; Resize.
  (bind-key "C-h" #'evil-window-decrease-width evil-window-map)
  (bind-key "C-j" #'evil-window-increase-height evil-window-map)
  (bind-key "C-k" #'evil-window-decrease-height evil-window-map)
  (bind-key "C-s" #'evil-window-increase-width evil-window-map)

  ;; Save/restore window configuration from register.
  (bind-key "m" #'window-configuration-to-register evil-window-map)
  (bind-key "'" #'jump-to-register evil-window-map)

  ;; Find file in a new window (useful because it starts in the current window's directory).
  (bind-key "f" #'find-file-other-window evil-window-map))

;; Undo (restore previous window configuration).
(use-package winner ; Bundled with Emacs.
  :demand t
  :init
  (setq winner-dont-bind-my-keys t) ; Don't bind C-c <left> or C-c <right>.
  :config
  (winner-mode 1)
  (with-eval-after-load 'evil
    (bind-key "u" #'winner-undo evil-window-map)
    (bind-key "C-r" #'winner-redo evil-window-map)))

(provide 'conf/view/windows/window-bindings)
