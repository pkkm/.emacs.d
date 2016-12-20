;;; Trailing whitespace settings. -*- lexical-binding: t -*-

;;; Automatically trim trailing whitespace in edited lines.

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  :config
  ;; Don't replace tabs with spaces, even when `indent-tabs-mode' is nil.
  (defadvice ws-butler-clean-region (around my-ws-butler-dont-untabify activate)
    (let ((indent-tabs-mode t))
      ad-do-it)))

;;; Highlight trailing whitespace in a buffer iff it's visiting a file.

(setq-default show-trailing-whitespace nil)

(defun show-trailing-whitespace-iff-visiting-file ()
  "Display trailing whitespace in the current buffer iff it's visiting a file.
Returns nil so that it can be added to hooks which terminate when a function returns t, e.g. `find-file-functions'."
  (setq show-trailing-whitespace (not (not buffer-file-name)))
  nil)

(require 'conf/utils/hooks) ; Used: add-hooks.
(add-hooks '(find-file-hook write-file-functions)
           #'show-trailing-whitespace-iff-visiting-file)

(provide 'conf/opening-saving/trailing-whitespace)
