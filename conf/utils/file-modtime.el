;;; Utilities for handling file modification times. -*- lexical-binding: t -*-

(defun file-modtime (file)
  "Get the last modification time of FILE.
If FILE cannot be read, return nil."
  (let ((attributes (file-attributes file)))
    (if attributes
        (nth 5 attributes)
      nil)))

(use-package f :ensure t :commands f-files)
(defun files-newer-than-time (directory time)
  "Return a list of files in DIRECTORY modified after TIME."
  (f-files directory
           (lambda (file)
             (time-less-p time (file-modtime file)))
           t))

(provide 'conf/utils/file-modtime)
