;;; Evil (Extensible VI Layer).

;; Make Evil's > and < always indent by `tab-width'.
;; This has to happen before loading Evil, because Evil makes `evil-shift-width' buffer local, and therefore impossible to declare as an alias.
;; I haven't found a way to reverse making variable buffer-local.
(defvaralias 'evil-shift-width 'tab-width)

(require 'conf/packages)
(package-ensure-installed 'evil)

(evil-mode 1)

(setq evil-cross-lines t) ; Allow motions to cross newlines.

(setq evil-regexp-search nil) ; Don't use regexes for / and ?.
(setq evil-flash-delay 999999) ; Number of seconds to highlight matches for.

;; Make C-u scroll instead of starting an argument.
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

;; In insert state, auto-indent the new line on a press of "RET" (or similar).
(mapcar (lambda (fun-to-remap)
          (define-key evil-insert-state-map (vector 'remap fun-to-remap) #'evil-ret-and-indent))
        '(newline newline-and-indent evil-ret))

(setq evil-want-C-w-delete t) ; Delete word in insert state with C-w.

(setq evil-echo-state nil) ; Don't echo state in the echo area (minibuffer).

;; Initial states in various modes.
(require 'cl-lib) ; Used: cl-loop.
(cl-loop for (mode state)
         in '((debugger-mode normal)
              (package-menu-mode normal))
         do (evil-set-initial-state mode state))

;; Unset keys that I'll later use as prefix keys.
(define-key evil-motion-state-map (kbd "SPC") nil)

(provide 'conf/evil)
