;;; Functions for handling strings.

(defun string-replace-first-regexp-match (regexp replacement string)
  "Replace the first match of REGEXP in STRING with REPLACEMENT.
If STRING doesn't contain REPLACEMENT, return STRING."
  (if (string-match regexp string)
      (replace-match replacement t t string)
    string))

(provide 'conf/utils/strings)
