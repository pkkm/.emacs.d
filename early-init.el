;;; Code executed before the package system is initialized. -*- lexical-binding: t -*-

;; Don't automatically call `package-initialize' before (Emacs 27+) or after (Emacs <27) `init.el'.
(setq package-enable-at-startup nil)
