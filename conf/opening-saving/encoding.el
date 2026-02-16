;;; Use UTF-8 by default, no matter the system we're running on. -*- lexical-binding: t -*-

;; To specify a coding argument for the next command, use:
;;   M-x universal-coding-system-argument RET
;; or:
;;   C-x RET c

;; Coding system for newly created buffers, I/O, etc.
(set-default-coding-systems 'utf-8-unix)

(provide 'conf/opening-saving/encoding)
