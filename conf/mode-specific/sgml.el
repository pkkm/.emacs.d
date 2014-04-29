;;; SGML (parent of HTML).

;;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-add-language-support sgml sgml-mode-hook
  ((sgml-indent-line . sgml-basic-offset)))
(smart-tabs-insinuate 'sgml)
(add-hook 'sgml-mode-hook #'enable-indent-tabs-mode)

;;; Misc.

(defun my-sgml-mode-customizations ()
  "My customizations to apply when `sgml-mode' is activated."
  (sgml-electric-tag-pair-mode 1)) ; When editing an opening tag, automatically update the closing tag.
(add-hook 'sgml-mode-hook #'my-sgml-mode-customizations)

;;; Emmet (formerly Zen Coding) -- expand abbreviations.

(package-ensure-installed 'emmet-mode)
(add-hook 'sgml-mode-hook #'emmet-mode)

;; Default binding: C-j

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

(setq emmet-move-cursor-after-expanding nil) ; Don't move the cursor, as the positions will be incorrect anyway due to the indent hack above.
(setq emmet-preview-default nil) ; Expand immediately, instead of asking for confirmation.

;; Disable the stupid highlight. The variable that's supposed to do this doesn't work.
(defadvice emmet-insert-and-flash (after remove-overlay activate)
  (emmet-remove-flash-ovl (current-buffer)))

(provide 'conf/mode-specific/sgml)
