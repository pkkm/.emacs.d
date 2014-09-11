;;; Main file of this Emacs config.

;; Ensure we're on Emacs 24.3 or newer.
(when (version< emacs-version "24.3")
  (error (concat "This config requires Emacs 24.3+. Current version: " emacs-version)))


;;; Directories.

;; Directories of this Emacs distribution.
(defvar main-dir user-emacs-directory
  "The root directory of my Emacs configuration.")
(setq package-user-dir (expand-file-name "elpa" main-dir)) ; The directory for `package.el' packages.
(defvar my-vendor-dir (expand-file-name "vendor" main-dir)
  "The directory for manually installed (non-`package.el') packages.")

;; Make Emacs think that the `.emacs.d' directory is `main-dir'/savefiles, so that packages don't clutter `main-dir' with their savefiles.
;; To get the path to a file inside this directory, use `locate-user-emacs-file' (this function is also used by packages to determine where they will save their files).
(setq user-emacs-directory (expand-file-name "savefiles/" main-dir)) ; The trailing slash is mandatory.

;; Add the (non-`package.el') packages in `my-vendor-dir' to `load-path'.
(add-to-list 'load-path my-vendor-dir)
(let ((default-directory my-vendor-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Add `main-dir' (the parent directory of conf/) to `load-path'.
(add-to-list 'load-path main-dir t) ; Add to the end of `load-path' so that .el files in `main-dir' don't shadow libraries.


;;; Package system.

;; Initialize packages now, instead of after init.
(package-initialize) ; This normally happens after loading the init file.
(setq package-enable-at-startup nil) ; Don't load the packages the second time after the init file.

;; MELPA.
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


;;; Syntactic sugar.

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


;;; Loading the rest of the config.

;; Config organization:
;;   Configuration is in conf/. The parent directory of conf/ is in the load path.
;;   (This enables configuration files to have feature names with a common prefix, e.g. 'conf/evil for evil.el, without the file names actually being prefixed.)
;;   .el files are `require'd in main.el and in each other (when there are dependencies).

;; Enable loading of the flattened configuration file if we're running from my USB drive, unless --no-flattened was passed.
(setq flattened-conf-file (locate-user-emacs-file "conf-flattened.el"))
(defvar load-flattened-conf (not (not (getenv "BUNDLE_ROOT"))) ; Double negation so that the variable is t or nil.
  "Should Emacs configuration be loaded from `flattened-conf-file' instead of the conf/ directory?")
(when (member "--no-flattened" command-line-args) ; We look for the argument manually because `command-switch-alist' is parsed after init.
  (setq command-line-args (delete "--no-flattened" command-line-args))
  (setq load-flattened-conf nil))

;; Load the normal or the flattened configuration.
(if (and load-flattened-conf
         (file-exists-p flattened-conf-file))
    (load (file-name-sans-extension flattened-conf-file))
  (require 'conf/main))
