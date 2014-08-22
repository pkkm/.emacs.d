;;; SGML (parent of HTML).

(require 'conf/packages)
(require 'conf/editing/indentation)

(use-package sgml-mode ; Bundled with Emacs.
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (smart-tabs-add-language-support sgml sgml-mode-hook
    ((sgml-indent-line . sgml-basic-offset)))
  (smart-tabs-insinuate 'sgml)
  (add-hook 'sgml-mode-hook #'enable-indent-tabs-mode)

  ;; When editing an opening tag, automatically update the closing tag.
  (add-hook 'sgml-mode-hook #'sgml-electric-tag-pair-mode))

;; Emmet (formerly Zen Coding) -- expand abbreviations like "p>ul>li*5".
;; Default binding: C-j
(use-package emmet-mode
  :ensure emmet-mode
  :commands emmet-mode
  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  :config

  ;; Emmet uses its own (braindead) indent code instead of the major mode's.
  ;; Let's use `indent-region' on every change to the buffer contents made by Emmet.
  (defadvice emmet-insert-and-flash (around remove-indent activate)
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

(provide 'conf/mode-specific/sgml)
