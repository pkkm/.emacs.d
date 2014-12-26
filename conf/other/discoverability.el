;;; Discoverability features.

;; When I press a prefix key, pop up a list of commands.
(use-package guide-key
  :ensure guide-key
  :defer t
  :diminish guide-key-mode
  :init
  (guide-key-mode 1)
  :config
  (setq guide-key/idle-delay 0.5)
  (setq guide-key/recursive-key-sequence-flag t) ; If `guide-key' is enabled for a keymap, also enable it for its subkeymaps.
  (setq guide-key/guide-key-sequence
        '("e" "C-x"
          "SPC" "C-c"
          "e RET" "C-x RET"
          "e C-n" "C-x n")))

;; Lots of additional help functionality (e.g. `describe-keymap').
(use-package help-fns+
  :ensure help-fns+
  :bind ("C-h M-k" . describe-keymap)) ; For autoloading.

;; Add magit-like menus to dired and some others.
(use-package discover
  :ensure discover
  :defer t
  :init
  (global-discover-mode 1)
  :config
  ;; Evil: make the menu buffer be in Emacs state.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'makey-key-mode 'emacs)))

;; C-h C-m -- show documentation for major mode commands (nicer than C-h m).
(use-package discover-my-major
  :ensure discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(provide 'conf/other/discoverability)
