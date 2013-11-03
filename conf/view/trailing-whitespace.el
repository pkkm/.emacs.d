;;; Highlight trailing whitespace in a buffer iff it is visiting a file.

(setq-default show-trailing-whitespace nil)

(defun show-trailing-whitespace-if-visiting-a-file ()
  "Turn on the display of trailing whitespace (`show-trailing-whitespace') in the current buffer if it is visiting a file.
This function always returns nil, so that it can be added to hooks like `find-file-functions', which terminate the operation if a function in them returns t."
  (interactive)
  (setq show-trailing-whitespace (not (not buffer-file-name)))
  nil)

(require 'conf/utils/hooks) ; Used: add-hooks.
(add-hooks '(find-file-hook write-file-functions)
           #'show-trailing-whitespace-if-visiting-a-file)

(provide 'conf/view/trailing-whitespace)
