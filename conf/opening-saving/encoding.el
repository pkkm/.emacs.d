;;; Use UTF-8 by default, no matter the system we're running on. -*- lexical-binding: t -*-

;; To specify a coding argument for the next command, use:
;;   M-x universal-coding-system-argument RET
;; or:
;;   C-x RET c

;; Coding system for newly created buffers, I/O, etc.
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; This probably should be left for Emacs to detect.
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
;;(set-language-environment 'utf-8)

;; This almost certainly should be left for Emacs to detect.
;;(setq locale-coding-system 'utf-8)

(provide 'conf/opening-saving/encoding)
