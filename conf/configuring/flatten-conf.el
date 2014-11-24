;;; Flatten the configuration in conf/ into a single .el file.

;; Note that the configuration is treated as a DAG.
;; It's guaranteed that if file A `require's B and C, then B and C will come before A in the flattened configuration. However, other than that, the order of B and C is undefined. Therefore, if you want to use a file in many config files, either `require' it in each file or move its code to init.el; don't `require' it only at the top of main.el.

(require 'conf/utils/strings) ; Used: string-starts-with.
(use-package dash :ensure dash :commands (-filter -uniq ->>))
(require 'conf/utils/load-history) ; Used: feature-requires-recursively.
(defun conf-load-order ()
  "Returns a list of features required by 'conf/main, loaded in this Emacs session, topologically sorted."
  (->> (feature-requires-recursively 'conf/main)
    (-filter (lambda (feature)
               (string-starts-with (symbol-name feature) "conf/")))
    (-uniq)))

(defvar flattened-conf-file
  (locate-user-emacs-file "conf-flattened.el")
  "Name of the file in which to store the flattened contents of the conf/ directory.")

(defun flatten-conf ()
  "Write the code in conf/ that gets executed when 'conf/main is required to `flattened-conf-file'."
  (interactive)
  (with-temp-file flattened-conf-file
    (insert ";; -*- byte-compile-warnings: (not free-vars unresolved) -*-") (newline)
    (insert ";; ^ Don't warn about things that are normal when configuring modes before they are loaded.") (newline)
    (insert ";;; This file is the result of flattening the Emacs configuration in conf/.") (newline)
    (insert ";;; It was automatically generated.") (newline)
    (newline) (newline)
    (mapc (lambda (conf-feature)
            (insert (concat ";;;; " (symbol-name conf-feature)))
            (newline) (newline)
            (let ((num-inserted-chars (cadr (insert-file-contents (feature-file conf-feature)))))
              (goto-char (+ (point) num-inserted-chars)))
            (newline) (newline) (newline))
          (conf-load-order))))

(require 'conf/utils/file-modtime) ; Used: any-file-in-directory-newer-than-p, file-modtime.
(defun reflatten-and-recompile-conf ()
  "If any file in conf/ is newer than the file `flattened-conf-file', re-flatten the configuration.
Then, byte-recompile the file."
  (interactive)
  (when (or (not (file-exists-p flattened-conf-file))
            (any-file-in-directory-newer-than-p (expand-file-name "conf" main-dir)
                                                (file-modtime flattened-conf-file)))
    (message "Flattening conf/...")
    (flatten-conf)
    (message "Flattening conf/... done."))
  (require 'bytecomp) ; Autoload won't work for `byte-compile-dest-file', because the definition isn't at top-level.
  (let ((compiled-flattened-conf (byte-compile-dest-file flattened-conf-file)))
    (when (or (not (file-exists-p compiled-flattened-conf))
              (file-newer-than-file-p flattened-conf-file compiled-flattened-conf))
      (message "Compiling flattened conf/...")
      (if (byte-compile-file flattened-conf-file)
          (message "Compiling flattened conf/... done.")
        (message "Compiling flattened conf/... error.")))))

;; Run the above each time Emacs is idle for a certain amount of time.
(defvar reflatten-and-recompile-conf-idle-timeout 240 ; 4 minutes.
  "After how many idle seconds should conf/ be flattened and compiled?")
(run-with-idle-timer reflatten-and-recompile-conf-idle-timeout t
                     #'reflatten-and-recompile-conf)

(provide 'conf/configuring/flatten-conf)
