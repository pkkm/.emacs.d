;;; Windows. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  ;; Split window and display the next buffer.
  (defun my-split-window-right ()
    (interactive)
    (call-interactively #'evil-window-vsplit)
    (switch-to-buffer (other-buffer)))
  (defun my-split-window-below ()
    (interactive)
    (call-interactively #'evil-window-split)
    (switch-to-buffer (other-buffer)))

  (evil-define-key 'motion 'global (kbd "TAB") 'evil-window-map)
  (evil-define-key 'motion 'global (kbd "<tab>") 'evil-window-map) ; Needed so that Org (and maybe other modes) doesn't override this.

  ;; Split (with count: leave COUNT lines in the initially-selected window).
  (bind-key "d" #'my-split-window-right evil-window-map)
  (bind-key "s" #'my-split-window-below evil-window-map)

  ;; Close.
  (bind-key "K" #'evil-delete-buffer evil-window-map) ; Kill buffer and window.

  ;; Next, previous.
  (bind-key "TAB" #'evil-window-next evil-window-map)
  (bind-key "<backtab>" #'evil-window-prev evil-window-map) ; <backtab> -- S-TAB.

  ;; Select the window with the most recently used buffer (same as C-w C-p).
  (bind-key "SPC" #'evil-window-mru evil-window-map)

  ;; Resize.
  (bind-key "C-h" #'evil-window-decrease-width evil-window-map)
  (bind-key "C-j" #'evil-window-increase-height evil-window-map)
  (bind-key "C-k" #'evil-window-decrease-height evil-window-map)
  (bind-key "C-l" #'evil-window-increase-width evil-window-map)

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
