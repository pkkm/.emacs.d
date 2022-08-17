;;; Main file of this Emacs config. -*- lexical-binding: t -*-

;; Ensure we're on Emacs 25.1 or newer.
(when (version< emacs-version "25.1")
  (error "This config requires Emacs 25.1+. Current version: %s" emacs-version))

;; Load `early-init.el' on Emacs 27+.
(when (version<= "27.1" emacs-version)
  (load (expand-file-name "early-init" user-emacs-directory)))

;; Improve startup time by temporarily changing some settings.
(let ((old-gc-cons-percentage gc-cons-percentage)
      (old-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 1)
  (setq file-name-handler-alist nil) ; Disable handling of compressed/encrypted/TRAMP files.
  (defun restore-performance-settings ()
    (setq gc-cons-threshold (* 32 1024 1024))
    (setq gc-cons-percentage old-gc-cons-percentage)
    (setq file-name-handler-alist old-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'restore-performance-settings))

;; Don't try to use an external TLS program on Windows (it won't work).
(setq my-use-tls (or (not (eq system-type 'windows-nt)) (gnutls-available-p)))

;; Increase TLS security. To test this, run `test-https-verification' from `conf/utils/https'. See <https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00718.html>.
(setq network-security-level 'high)

;; Work around security issues (see <https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS?h=emacs-25>).
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end)))
  (setq tls-program '("gnutls-cli --x509cafile %t -p %p %h")))

;; Work around a TLS bug (see <https://old.reddit.com/r/emacs/comments/ct0h6m>, <https://github.com/magit/forge/issues/152>).
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Silence the warning "Package cl is deprecated" (Emacs 27+).
;; This is intented to affect `do-after-load-evaluation' in `subr'.
(when (version<= "27.1" emacs-version)
  (defadvice byte-compile-warning-enabled-p (after disable-obsolete-cl-warning activate)
    (when (equal (ad-get-args 0) '(obsolete cl))
      (setq ad-return-value nil))))


;;; Directories.

;; Directories of this Emacs distribution.
(defvar main-dir user-emacs-directory
  "The root directory of my Emacs configuration.")
(setq package-user-dir (expand-file-name "elpa" main-dir))
(defvar my-elpa-repo-dir (expand-file-name "repo" main-dir)
  "The directory for my private ELPA mirror.")
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

;; To update installed packages, use M-x package-list-packages RET U x.
;; Or delete the elpa/ directory and launch Emacs for it to be recreated.

;; Initialize packages now, instead of after init.
;; Automatic initialization is disabled in `early-init.el'.
(package-initialize)

;; Clone my private repo if it's not found and git is available.
(when (and (not (file-exists-p my-elpa-repo-dir)) (executable-find "git"))
  (message "Cloning my private ELPA...")
  (call-process
   "git" nil "*git clone my-elpa output*" t
   "clone" "https://github.com/pkkm/my-elpa" my-elpa-repo-dir))

;; Package archives.
(let ((proto (if my-use-tls "https://" "http://")))
  (setq package-archives
        (list (cons "gnu" (concat proto "elpa.gnu.org/packages/"))
              (cons "melpa-stable" (concat proto "stable.melpa.org/packages/"))
              (cons "melpa" (concat proto "melpa.org/packages/")))))
(when (file-exists-p (expand-file-name "archive-contents" my-elpa-repo-dir))
  (push (cons "my-elpa-repo" my-elpa-repo-dir) package-archives))
(setq package-archive-priorities
      '(("my-elpa-repo" . 3)
        ("gnu" . 2)
        ("melpa-stable" . 1)
        ("melpa" . 0)))

;; Make sure we're using current GNU ELPA signing keys.
(when (version< emacs-version "26.3")
  (let ((ts (expand-file-name "gnu-elpa.timestamp"
                              (or (bound-and-true-p package-gnupghome-dir) ; Introduced in Emacs 26.1.
                                  (expand-file-name "gnupg" package-user-dir))))
        (kr (expand-file-name "gnu-elpa.gpg-keyring" my-vendor-dir)))
    (when (file-newer-than-file-p kr ts)
      (package-import-keyring kr)
      (write-region "" nil ts nil 'silent))))

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
           (let ((pkg-installed (car (epl-find-installed-packages pkg-symbol))))
             (when (or (null pkg-installed)
                       (version< (epl-package-version-string pkg-installed) min-version))
               (message "Upgrading package %s (required version: %s)." pkg-symbol min-version)
               (unless packages-refreshed-this-session-p
                 (package-refresh-contents))
               (let ((pkg-available (car (epl-find-available-packages pkg-symbol))))
                 (unless pkg-available
                   (error "Package %s not available for installation" pkg-symbol))
                 (epl-package-install pkg-available))
               ;; Reload package if loaded.
               (when (featurep pkg-symbol)
                 (unload-feature pkg-symbol)
                 (require pkg-symbol))))))

;; Get `abbrev-mode' out of the way.
(setq save-abbrevs 'silently) ; Don't ask whether to save abbrevs during package installation.
(setq abbrev-file-name (locate-user-emacs-file "abbrev_defs")) ; Don't litter `.emacs.d'.


;;; Syntactic sugar.

;; Easily disable showing certain modes in the modeline.
(package-ensure-installed 'diminish)

;; Easily define personal keybindings.
;; Use `describe-personal-keybindings' to show all bindings defined using `bind-key'.
(package-ensure-installed 'bind-key)
(autoload 'bind-key "bind-key")

;; Configure packages in an elegant and performant way.
(package-ensure-installed 'use-package)
(setq use-package-always-defer t) ; Assume `:defer t' by default.
(eval-when-compile
  (require 'use-package))

;; Modern list library (used often in this config).
(use-package dash :ensure t :demand t)


;;; Load the rest of the config.
;; Configuration is in conf/. The parent directory of conf/ is in the load path. (This enables configuration files to have feature names with a common prefix, e.g. 'conf/evil for evil.el, without the file names actually being prefixed.)

;; Store Custom's settings in a separate file instead of writing them here.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(use-package f :ensure t :demand t)
(dolist (file (f-files (expand-file-name "conf" main-dir)
                       (lambda (file) (f-ext? file "el"))
                       t))
  (let ((feature-name (f-no-ext (f-relative file main-dir))))
    (require (intern feature-name))))
