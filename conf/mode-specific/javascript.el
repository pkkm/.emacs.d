;;; Javascript. -*- lexical-binding: t -*-

;; Javascript IDE.
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-highlight-level 3)) ; Highlight many built-in functions.

;; Skewer -- live web development minor mode.
;; Methods of launching:
;;   * M-x run-skewer
;;   * Use the Skewer bookmarklet to inject it into an existing page.
;; Keybindings resemble the Lisp ones:
;;   C-x C-e -- JS: eval form (with prefix: insert result); CSS: load declaration.
;;   C-M-x -- JS: eval top-level-form; CSS: load rule; HTML: load tag.
;;   C-c C-k -- JS, CSS: eval buffer.
;;   C-c C-z -- JS: switch to REPL (logging: "skewer.log()", like "console.log()").
;; Forms are sent to all attached clients simultaneously (use `list-skewer-clients' to show them).
;; If the browser disconnects, use "skewer()" in the browser console to reconnect.
(use-package skewer-mode
  :ensure t
  :init
  (skewer-setup)) ; Integrate with js2-mode, html-mode and css-mode. (Don't worry about performance, this function is in a separate file.)

;; Refactoring (and many common operations, e.g. kill expression).
;; Integrates nicely with `emacs-refactor' (select some code before invoking it).
(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(provide 'conf/mode-specific/javascript)
