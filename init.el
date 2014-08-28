;;; Main file of this Emacs config.

;; Directories of this Emacs distribution.
(defvar main-dir user-emacs-directory
  "The root directory of my Emacs configuration.")
(defvar package-user-dir (expand-file-name "elpa" main-dir)
  "The directory for `package.el' packages.")
(defvar my-vendor-dir (expand-file-name "vendor" main-dir)
  "The directory for manually installed (non-`package.el') packages.")
(defvar my-savefile-dir (expand-file-name "savefiles" main-dir)
  "The directory for automatically generated save/history/etc. files.")

;; Add the (non-`package.el') packages in `my-vendor-dir' to `load-path'.
(add-to-list 'load-path my-vendor-dir)
(let ((default-directory my-vendor-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Add `main-dir' (the parent directory of conf/) to `load-path'.
(add-to-list 'load-path main-dir t) ; Add to the end of `load-path' so that .el files in `main-dir' don't shadow libraries.

;; Config organization:
;;   Configuration is in conf/. The parent directory of conf/ is in the load path.
;;   (This enables configuration files to have feature names with a common prefix, e.g. 'conf/evil for evil.el, without the file names actually being prefixed.)
;;   .el files are `require'd in main.el and in each other (when there are dependencies).

;; Load the flattened configuration file if we're running from my USB drive.
(defvar load-flattened-conf (not (not (getenv "BUNDLE_ROOT"))) ; Double negation so that the variable is t or nil.
  "Should Emacs configuration be loaded from `flattened-conf-file' instead of the conf/ directory?")
(setq flattened-conf-file (expand-file-name "conf-flattened.el" my-savefile-dir))

(if (and load-flattened-conf
         (file-exists-p flattened-conf-file))
    (load (file-name-sans-extension flattened-conf-file))
  (require 'conf/main))
