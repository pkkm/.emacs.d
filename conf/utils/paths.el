;;; Utilities for handling paths.

(defun shorten-path (path max-length)
  "Shorten PATH to up to MAX-LENGTH characters."
  (let ((reversed-path-list (reverse (split-string (abbreviate-file-name path) "/")))
        (output "")
        (prefix-when-shortened "…/"))
    (when reversed-path-list
      (when (equal "" (car reversed-path-list)) ; Ignore trailing slash.
        (setq reversed-path-list (cdr reversed-path-list)))
      (let ((new-path))
        (while (and reversed-path-list
                    (setq new-path (concat (car reversed-path-list) "/" output)) ; The return value doesn't matter (it's always non-nil).
                    (<= (+ (length new-path) (length prefix-when-shortened))
                        max-length))
          (setq output new-path)
          (setq reversed-path-list (cdr reversed-path-list))))
      (when reversed-path-list
        (setq output (concat prefix-when-shortened output))))
    output))

(provide 'conf/utils/paths)
