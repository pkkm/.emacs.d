;;; Main file of this Emacs config. -*- lexical-binding: t -*-

;; Ensure we're on Emacs 24.4 or newer.
(when (version< emacs-version "24.4")
  (error (concat "This config requires Emacs 24.4+. Current version: " emacs-version)))

;; Optimization: don't do many small garbage collections during init.
(let ((old-gc-cons-threshold gc-cons-threshold))
  (setq gc-cons-threshold (* 128 1024 1024))
  (defun restore-default-gc-settings ()
    (garbage-collect)
    (setq gc-cons-threshold old-gc-cons-threshold))
  (add-hook 'emacs-startup-hook #'restore-default-gc-settings))


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

;; Workaround for Windows: replace "//" with "/" in `exec-path'.
;; (<drive-letter>://<path> causes Emacs to ignore the drive.)
;; TODO: fix the cause of such paths (Prepare.sh?).
(when (eq system-type 'windows-nt)
  (defun sanitize-path (path)
    (replace-regexp-in-string "//" "/" path))
  (setq exec-path (mapcar #'sanitize-path exec-path)))


;;; Package system.

;; Initialize packages now, instead of after init.
(package-initialize) ; This normally happens after loading the init file.
(setq package-enable-at-startup nil) ; Don't load the packages the second time after the init file.

;; Package archives.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

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

(package-ensure-installed 'epl)
(require 'epl)
(eval-when-compile (require 'cl-lib))
(defun package-ensure-version (&rest package-version-plist)
  "(package-ensure-version [PACKAGE MIN-VERSION]...)
If a PACKAGE (as a symbol) is older than MIN-VERSION, install its newest version."
  (cl-loop for (pkg-symbol min-version)
           on package-version-plist by #'cddr
           do
           (let ( ; The following `epl' functions return packages sorted by version descending.
                 (pkg-installed (car (epl-find-installed-packages pkg-symbol)))
                 (pkg-available (car (epl-find-available-packages pkg-symbol))))
             (unless (or pkg-installed pkg-available)
               (error "Package %s neither installed nor available" pkg-symbol))
             (when (or (null pkg-installed)
                       (version< (epl-package-version-string pkg-installed) min-version))
               (message "Upgrading package %s (required version: %s)." pkg-symbol min-version)
               (epl-package-install pkg-available)
               ;; Reload package if loaded.
               (when (featurep pkg-symbol)
                 (unload-feature pkg-symbol)
                 (require pkg-symbol))))))


;;; Syntactic sugar.

;; Easily disable showing certain modes in the modeline.
(package-ensure-installed 'diminish)

;; Easily define personal keybindings.
;; Use `describe-personal-keybindings' to show all bindings defined using `bind-key'.
(package-ensure-installed 'bind-key)
(autoload 'bind-key "bind-key")

;; Configure packages in a tidy, performance-oriented way.
(package-ensure-version 'use-package "20160226") ; Can be any version after `use-package-always-defer' was introduced.
(setq use-package-always-defer t) ; Assume `:defer t' by default.
(eval-when-compile
  (require 'use-package))

;; Modern list library (used often in this config).
(use-package dash
  :ensure t
  :demand t)


;;; Load the rest of the config.
;; Configuration is in conf/. The parent directory of conf/ is in the load path. (This enables configuration files to have feature names with a common prefix, e.g. 'conf/evil for evil.el, without the file names actually being prefixed.)

(use-package f :ensure t :commands (f-files f-no-ext f-relative f-ext?))
 (mapc (lambda (file)
         (let ((feature-name (f-no-ext (f-relative file main-dir))))
           (require (intern feature-name))))
       (f-files (expand-file-name "conf" main-dir)
                (lambda (file) (f-ext? file "el"))
                t))
