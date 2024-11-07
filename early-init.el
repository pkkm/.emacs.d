;;; Code executed before the package system is initialized. -*- lexical-binding: t -*-

;; Disable native compilation (I'd rather wait until it's better tested by people).
(setq native-comp-deferred-compilation nil)
(setq native-comp-speed -1)

;; Don't automatically call `package-initialize' before `init.el'.
(setq package-enable-at-startup nil)
