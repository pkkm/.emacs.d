;;; Utilities for handling file modification times.

(defun file-modtime (file)
  "Get the last modification time of FILE.
If FILE cannot be read, return nil."
  (let ((attributes (file-attributes file)))
    (if attributes
        (nth 5 attributes)
      nil)))

(defun file-modified-later-p (file-a file-b)
  "Return t if FILE-A was modified later than FILE-B.
If FILE-B cannot be read, return t. If FILE-A cannot be read, nil."
  (let ((modtime-a (file-modtime file-a))
        (modtime-b (file-modtime file-b)))
    (cond
     ((not modtime-b) t)
     ((not modtime-a) nil)
     (t (time-less-p modtime-b modtime-a)))))

(provide 'conf/utils/file-modtime)
