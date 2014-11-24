;;; Functions for handling strings.

(defun string-replace-first (old new string)
  "Replace the first occurrence of OLD with NEW in STRING."
  (if (string-match (regexp-quote old) string)
      (replace-match new t t string)
    string))

(provide 'conf/utils/strings)
