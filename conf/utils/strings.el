;;; Functions for handling strings.

(defun string-replace-first (old new string)
  "Replace the first occurrence of OLD with NEW in STRING."
  (if (string-match (regexp-quote old) string)
      (replace-match new t t string)
    string))

(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

(provide 'conf/utils/strings)
