;;; Utilities for extracting information from the `load-history' variable.

(require 'cl-lib)
(autoload 'file-requires "loadhist")
(autoload 'feature-file "loadhist")

(defun feature-requires-recursively (feature)
  "Returns the list of all features required by FEATURE, recursively.

Every feature is preceded by the features it requires (i.e. the list is topologically sorted).
There can be repetitions.

This function uses the variable `load-history' to get the information. It doesn't load FEATURE."
  (append
   (cl-mapcan (lambda (dependency)
                (feature-requires-recursively dependency))
              (file-requires (symbol-name feature)))
   (list feature)))

(provide 'conf/utils/load-history)
