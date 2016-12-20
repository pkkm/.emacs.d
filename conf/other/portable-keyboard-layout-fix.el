;;; A fix for Portable Keyboard Layout (the program I use to get Programmer Dvorak on other people's computers) sending C-<number> instead of C-<symbol>. -*- lexical-binding: t -*-

(require 'cl-lib) ; Used: cl-loop.

(cl-loop for (pkl-key . real-key)
         in '(("C-7" . "C-[")
              ("C-5" . "C-{")
              ("C-3" . "C-}")
              ("C-1" . "C-(")
              ("C-9" . "C-=")
              ("C-0" . "C-*")
              ("C-2" . "C-)")
              ("C-4" . "C-+")
              ("C-6" . "C-]")
              ("C-8" . "C-!"))
         do
         (define-key input-decode-map (kbd pkl-key) (kbd real-key))) ; We can't use `key-translation-map' since global bindings apparently take precedence over it

(provide 'conf/other/portable-keyboard-layout-fix)
