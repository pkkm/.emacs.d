;;; Utilities for handling file modification times.

(defun file-modtime (file)
  "Get the last modification time of FILE.
If FILE cannot be read, return nil."
  (let ((attributes (file-attributes file)))
    (if attributes
        (nth 5 attributes)
      nil)))

(use-package dash :ensure dash) ; Used: -any?.
(defun any-file-in-directory-newer-than-p (directory time)
  "Returns t if any file in DIRECTORY (recursively) has been modified after TIME, otherwise nil."
  (-any? (lambda (file)
           (unless (member (file-relative-name file directory) (list "." ".."))
             (if (file-directory-p file)
                 (any-file-in-directory-newer-than-p file time)
               (time-less-p time (file-modtime file)))))
         (directory-files directory 'absolute-file-names)))

(provide 'conf/utils/file-modtime)
