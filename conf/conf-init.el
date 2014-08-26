;;; Set up (mostly package-related) things used in almost all of my configuration.
;; This is the first file required by main.el.

;; Ensure we're on Emacs 24.3 or newer.
(when (version< emacs-version "24.3")
  (error (concat "This config requires Emacs 24.3+. Current version: " emacs-version)))

;; Initialize packages now, instead of after init.
(package-initialize) ; This normally happens after loading the init file.
(setq package-enable-at-startup nil) ; Don't load the packages the second time after the init file.

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; To update installed packages, use M-x package-list-packages RET U x.
;; Or delete the elpa/ directory and launch Emacs for it to be recreated.

;; First time that `package-install' is called in this session, refresh the package list (if it wasn't already refreshed).
(defadvice package-install (before refresh-before-install activate)
  "Refresh the package list before installing a new package.
This will happen at most once per session, as this advice is removed when `package-refresh-contents' is called."
  (package-refresh-contents))
(defadvice package-refresh-contents (before disable-refresh-before-install activate)
  "Remove the advice to `package-install' that refreshes the package list, then self-destruct."
  (ad-remove-advice 'package-install 'before 'refresh-before-install)
  (ad-update 'package-install) ; Necessary for the above change to take effect.
  (ad-remove-advice 'package-refresh-contents 'before 'disable-refresh-before-install)
  (ad-update 'package-refresh-contents))

(defun package-ensure-installed (package)
  "Ensure the ELPA package PACKAGE is installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Easily disable showing certain modes in the modeline.
(package-ensure-installed 'diminish)

;; Easily define personal keybindings.
;; Use `describe-personal-keybindings' to show all bindings defined using `bind-key'.
(package-ensure-installed 'bind-key)
(autoload 'bind-key "bind-key")

;; Configure packages in a tidy, performance-oriented way.
(package-ensure-installed 'use-package)
(require 'use-package) ; If `use-package' is autoloaded, this file produces an error when its compiled version is loaded.

(provide 'conf/conf-init)
