;;; Tiny tweaks that don't deserve their own file.

;; Show character names in `C-x =`.
(setq what-cursor-show-names t)

;; Enable commands that new users often find confusing.
(put 'narrow-to-region 'disabled nil)

;; Disable minimizing graphical frames (bound to C-z, C-x C-z, etc.).
;; Every time I did this, it was by accident, and it led to lost windows (i3 hides minimized windows with no way to restore them).
(defadvice iconify-frame (around disable-iconifying activate)
  (message "Not minimizing frame."))

;; An alternative way of entering modified keys unavailable in terminals, e.g. C-}.
(bind-key "M-c" nil) ; Otherwise it would be shadowed by `capitalize-word'.
(bind-key "M-c" #'event-apply-control-modifier function-key-map)

;; A binding for inserting a Unicode character by name.
(bind-key "C-c u" #'insert-char)

;; Break line at point. If we're in a comment, continue it.
(bind-key "<C-return>" #'indent-new-comment-line) ; Works both in GUI and xterm.

;; Make M-< go to the end of the prompt instead of the actual beginning of the minibuffer.
(setq minibuffer-beginning-of-buffer-movement t)

;; Default line wrapping width.
(setq-default fill-column 80)

;;; Use X's PRIMARY selection in addition to CLIPBOARD.
(setq x-select-enable-primary t)

(provide 'conf/misc)
