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
;; A variable is used instead of removing the advice using `ad-remove-advice' and `ad-update' because on Emacs 24.3 and earlier, removing an advice while it's executing causes an error.
(defvar packages-refreshed-this-session-p nil
  "Was the package list refreshed in this session?")
(defadvice package-install (before refresh-before-install activate)
  "Refresh the package list before installing a new package, if `packages-refreshed-this-session-p' is nil.
This will happen at most once per session, as `packages-refreshed-this-session-p' is set by an advice to `package-refresh-contents'."
  (unless packages-refreshed-this-session-p
    (package-refresh-contents)))
(defadvice package-refresh-contents (before set-packages-refreshed activate)
  "Set `packages-refreshed-this-session-p' to t."
  (setq packages-refreshed-this-session-p t))

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

;; Define `with-eval-after-load' if it's not present.
;; This is for compatibility with Emacs 24.3 (`with-eval-after-load' was introduced in 24.4).
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name, in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

(provide 'conf/conf-init)
