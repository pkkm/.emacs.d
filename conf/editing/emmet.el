;;; Emmet (formerly Zen Coding) -- expand abbreviations like "p>ul>li*5" into HTML/XML tags. -*- lexical-binding: t -*-

(use-package emmet-mode
  :ensure t
  :config

  ;; Keybindings.
  (defun my-emmet-expand-yas-or-line ()
    "Expand Emmet abbreviation. Use YASnippet if enabled, Emmet's own code otherwise."
    (interactive)
    (if (and (boundp 'yas-minor-mode) yas-minor-mode)
        (call-interactively #'emmet-expand-yas)
      (call-interactively #'emmet-expand-line)))
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap emmet-mode-keymap)
  (bind-key "C-j" #'my-emmet-expand-yas-or-line emmet-mode-keymap)
  (bind-key "C-c %" #'emmet-wrap-with-markup)) ; Default: C-c w.

(provide 'conf/editing/emmet)
