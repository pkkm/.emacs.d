;;; Code executed before the package system is initialized. -*- lexical-binding: t -*-

;; Disable native compilation (I'd rather wait until it's better tested by people).
(setq native-comp-jit-compilation nil)
(setq native-comp-speed -1)

;; Don't automatically call `package-initialize' before `init.el'.
(setq package-enable-at-startup nil)

;; Improve startup time by temporarily changing some settings.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold (* 256 1024 1024))
  (setq file-name-handler-alist nil) ; Disable handling of compressed/encrypted/TRAMP files.
  (defun my-restore-performance-settings ()
    (setq gc-cons-threshold (* 64 1024 1024))
    (setq file-name-handler-alist old-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'my-restore-performance-settings))
