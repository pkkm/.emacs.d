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
(dolist (fun-to-remap
         '(newline newline-and-indent evil-ret))
  (define-key evil-insert-state-map (vector 'remap fun-to-remap) #'evil-ret-and-indent))

(setq evil-want-C-w-delete t) ; Delete word in insert state with C-w.

(setq evil-echo-state nil) ; Don't echo state in the echo area (minibuffer).

;; Don't allow modes to change the definition of paragraph used by { and }.
(defun kill-local-paragraph-definitions ()
  "Kill the local values of `paragraph-start' and `paragraph-separate'."
  (kill-local-variable 'paragraph-start)
  (kill-local-variable 'paragraph-separate))
(add-hook 'after-change-major-mode-hook #'kill-local-paragraph-definitions)

;; Initial states in various modes.
;; TODO move to files with the configuration for the modes.
(evil-set-initial-state 'debugger-mode 'normal)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-set-initial-state 'compilation-mode 'motion)
(evil-set-initial-state 'occur-mode 'motion)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'compilation-mode 'emacs)

;; Don't allow any keymap to shadow Evil bindings.
(setq evil-overriding-maps '()) ; `evil-overriding-maps' get elevated to the "overriding" status -- above global state keymap, but below the local one.
(setq evil-pending-overriding-maps '()) ; No idea what the "pending" version is for, but setting only the above doesn't seem to work, for example in compilation-mode.
;;(setq evil-intercept-maps '()) ; `evil-intercept-maps' get elevated to the "intercept" status -- above all others. This is useful when debugging.

;; Unset keys that I'll later use as prefix keys.
(define-key evil-motion-state-map (kbd "SPC") nil)

;; Unset other useless keys.
(define-key evil-insert-state-map (kbd "C-e") nil)

;; Don't display undo-tree-mode (used by evil) in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'undo-tree-mode "")

(provide 'conf/evil)
