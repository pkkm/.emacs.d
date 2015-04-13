;;; Discoverability features.

;; When I press a prefix key, pop up a list of commands.
(use-package guide-key
  :ensure t
  :defer t
  :diminish guide-key-mode
  :init
  (guide-key-mode 1)
  :config
  (setq guide-key/idle-delay 0.5)
  (setq guide-key/guide-key-sequence t)) ; Enable for all prefixes.

;; Lots of additional help functionality (e.g. `describe-keymap').
(use-package help-fns+
  :ensure t
  :bind ("C-h M-k" . describe-keymap)) ; For autoloading.

;; Add magit-like menus to dired and some others.
;; Disabled because it was too intrusive (e.g. bound M-s to its own command in every major mode's map) and I haven't found a way to override its bindings.
;;(use-package discover
;;  :ensure t
;;  :defer t
;;  :init
;;  (global-discover-mode 1)
;;  :config
;;  ;; Evil: make the menu buffer be in Emacs state.
;;  (with-eval-after-load 'evil
;;    (evil-set-initial-state 'makey-key-mode 'emacs)))

;; C-h C-m -- show documentation for major mode commands (nicer than C-h m).
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(provide 'conf/other/discoverability)
