;;; Utilities for handling file modification times. -*- lexical-binding: t -*-

(defun my-file-modtime (file)
  "Get the last modification time of FILE.
If FILE cannot be read, return nil."
  (file-attribute-modification-time (file-attributes file)))

(defun my-files-newer-than-time (directory time)
  "Return a list of files in DIRECTORY modified after TIME."
  (seq-filter
   (lambda (file)
     (let ((mtime (my-file-modtime file)))
       (and mtime (time-less-p time mtime))))
   (directory-files-recursively directory "")))

(provide 'conf/utils/file-modtime)
