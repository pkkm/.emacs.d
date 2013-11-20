;;; Functions for handling strings.

(defun string-replace-first-regexp-match (regexp replacement string)
  "Replace the first match of REGEXP in STRING with REPLACEMENT.
If STRING doesn't contain REPLACEMENT, return STRING."
  (if (string-match regexp string)
      (replace-match replacement t t string)
    string))

(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

(provide 'conf/utils/strings)
