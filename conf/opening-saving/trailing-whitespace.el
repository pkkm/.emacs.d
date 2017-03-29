;;; Trailing whitespace settings. -*- lexical-binding: t -*-

;;; Automatically trim trailing whitespace in edited lines.

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;;; Highlight trailing whitespace in a buffer iff it's visiting a file.

(setq-default show-trailing-whitespace nil)

(defun my-set-show-trailing-whitespace ()
  "Show trailing whitespace iff visiting a file (unless overridden via a file local variable).
Returns nil so that it can be added to hooks which terminate when a function returns t, e.g. `find-file-functions'."
  (unless (assq 'show-trailing-whitespace file-local-variables-alist)
    (setq show-trailing-whitespace (not (not buffer-file-name))))
  nil)

(require 'conf/utils/hooks) ; Used: add-hooks.
(add-hooks '(find-file-hook write-file-functions)
           #'my-set-show-trailing-whitespace)

(provide 'conf/opening-saving/trailing-whitespace)
