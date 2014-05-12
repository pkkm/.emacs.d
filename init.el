;;; Main file of this Emacs config.

;; Directories of this Emacs distribution.
(defvar main-dir (file-name-directory load-file-name) ; The directory of this file.
  "The root directory of my Emacs distribution.")
(defvar package-user-dir (expand-file-name "elpa" main-dir)
  "The directory for ELPA archives (and data).")
(defvar my-vendor-dir (expand-file-name "vendor" main-dir)
  "The directory for packages not yet available in ELPA.")
(defvar my-savefile-dir (expand-file-name "savefiles" main-dir)
  "The directory for automatically generated save/history/etc. files.")

;; Add the (non-ELPA) packages in `my-vendor-dir` to `load-path`.
(let ((default-directory my-vendor-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Config organization:
;;   Configuration is in conf/. The parent directory of conf/ is in the load path.
;;   (This enables configuration files to have feature names with a common prefix,
;;   for example 'conf/evil for evil.el, without the file names actually being prefixed.)
;;   .el files are `require`d in main.el/g and in each other (when there are dependencies).

;; Add main-dir (the parent directory of conf/) to the load path.
(let ((default-directory main-dir))
  (normal-top-level-add-to-load-path '(".")))

;; Load the flattened configuration file if we're running from my USB drive.
(defvar load-flattened-conf (not (not (getenv "BUNDLE_ROOT"))) ; Double negation so that the variable is t or nil.
  "Should Emacs configuration be loaded from `flattened-conf-file' instead of the conf/ directory?")
(setq flattened-conf-file (expand-file-name "conf-flattened.el" my-savefile-dir))

(if (and load-flattened-conf
         (file-exists-p flattened-conf-file))
    (load (file-name-sans-extension flattened-conf-file))
  (require 'conf/main))

;; Previously used method of organizing my config:
;;   Organize in conf/ (and subdirectories) and use the below function to load in glob order.
;; Problem:
;;   Many files depend on others and the order had to be manually managed.
;;(defun load-recursively (dir-to-load &optional exclude-list)
;;  "Load all .el files in DIR-TO-LOAD and its subdirectories (and their subdirectories, ...).
;;Exclude files (or directories) that are in EXCLUDE-LIST."
;;  (let ((excluded-file-names (append exclude-list '("." ".."))))
;;    (dolist (file (directory-files dir-to-load 'absolute-file-names))
;;      (unless (member (file-relative-name file dir-to-load) excluded-file-names)
;;        (cond
;;          ((file-directory-p file) (load-recursively file exclude-list))
;;          ((string= (file-name-extension file) "el") (load file)))))))
;;(load-recursively my-conf-dir)
