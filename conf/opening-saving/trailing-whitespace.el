;;; Trailing whitespace settings. -*- lexical-binding: t -*-

;;; Automatically trim trailing whitespace in edited lines.

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;;; Highlight trailing whitespace in a buffer iff it's visiting a file.

(setq-default show-trailing-whitespace nil)

(defun my-show-trailing-whitespace-iff-visiting-file ()
  "Show trailing whitespace iff visiting a file, unless overridden via a file local variable."
  (unless (assq 'show-trailing-whitespace file-local-variables-alist)
    (setq show-trailing-whitespace (not (not (buffer-file-name))))))

(require 'conf/utils) ; Used: add-hooks.
(add-hooks '(find-file-hook after-set-visited-file-name-hook)
           #'my-show-trailing-whitespace-iff-visiting-file)

(provide 'conf/opening-saving/trailing-whitespace)
