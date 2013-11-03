;;; Emmet (formerly Zen Coding) -- expand abbreviations into structures of SGML tags.

(require 'conf/packages)
(package-ensure-installed 'emmet-mode)

;; Emmet uses its own (braindead) indent code instead of the major mode's.
;; Let's use `indent-region' on every change to the buffer contents made by Emmet.
(defadvice emmet-insert-and-flash (around remove-indent activate)
  "Automatically indent markup inserted by Emmet."
  (add-hook 'after-change-functions #'indent-changed-region)
  (unwind-protect
      (progn ad-do-it)
    (remove-hook 'after-change-functions #'indent-changed-region)))
(defun indent-changed-region (start end length)
  "Indent the changed region. To be called from the `after-change-functions' hook."
  (indent-region start end))

(setq emmet-move-cursor-after-expanding nil) ; Don't move the cursor, as the positions will be incorrect anyway due to the indent code above.
(setq emmet-preview-default nil) ; Expand immediately, instead of asking for confirmation.

;; Disable the stupid highlight.
;; The variable that's supposed to do this doesn't work.
(defadvice emmet-insert-and-flash (after remove-overlay activate)
  (emmet-remove-flash-ovl (current-buffer)))

;; Default binding for expand: C-j

(provide 'conf/other/emmet)
