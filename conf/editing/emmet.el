;;; Emmet (formerly Zen Coding) -- expand abbreviations like "p>ul>li*5" into HTML/XML tags.

(use-package emmet-mode
  :ensure emmet-mode
  :defer t
  :config

  ;; Keybindings: expand abbreviations with C-j, disable everything else.
  (defun my-emmet-expand-yas-or-line ()
    "Expand Emmet abbreviation. Use YASnippet if enabled, Emmet's own code otherwise."
    (interactive)
    (if (and (boundp 'yas-minor-mode) yas-minor-mode)
        (call-interactively #'emmet-expand-yas)
      (call-interactively #'emmet-expand-line)))
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap emmet-mode-keymap)
  (bind-key "C-j" #'my-emmet-expand-yas-or-line emmet-mode-keymap)


  ;;; Fixes for Emmet's expansion code (which is used for `emmet-expand-line', but not for `emmet-expand-yas').

  ;; Emmet uses its own (braindead) indent code instead of the major mode's.
  ;; Let's use `indent-region' on every change to the buffer contents it makes.
  (defadvice emmet-insert-and-flash (around my-reindent activate)
    "Automatically indent markup inserted by Emmet."
    (add-hook 'after-change-functions #'my-indent-region)
    (unwind-protect
        (progn ad-do-it)
      (remove-hook 'after-change-functions #'my-indent-region)))
  (defun my-indent-region (start end length)
    "Indent the region from START to END.
This function was written for use in `after-change-functions' and ignores the third argument."
    (indent-region start end))
  (setq emmet-move-cursor-after-expanding nil) ; Don't move the cursor, as the positions will be incorrect anyway due to the hack above.

  ;; Expand immediately, instead of asking for confirmation.
  (setq emmet-preview-default nil)

  ;; Disable highlighting inserted text. The variable that's supposed to do this doesn't work.
  (defadvice emmet-insert-and-flash (after remove-overlay activate)
    (emmet-remove-flash-ovl (current-buffer))))


(provide 'conf/editing/emmet)
