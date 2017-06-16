;;; Discoverability features. -*- lexical-binding: t -*-

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom) ; Show on right if there's enough space, on bottom otherwise.
  (setq which-key-idle-delay 0.5))

;; Lots of additional help functionality (e.g. `describe-keymap').
(use-package help-fns+ ; Installed in `my-vendor-dir' because the MELPA package is fetched from the wiki, which is insecure (as of 2017-06).
  :bind ("C-h M-k" . describe-keymap)) ; For autoloading.

;; C-h C-m -- show documentation for major mode commands (nicer than C-h m).
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

;; I tried to use discover.el but it was too intrusive (e.g. bound M-s to its own command in every major mode's map) and I haven't found a way to override its bindings.

(provide 'conf/other/discoverability)
