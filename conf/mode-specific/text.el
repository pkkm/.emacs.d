;;; Text mode.
;;; Parent of major modes for editing text for humans to read (Asciidoc, TeX, ...).

(add-hook
  'text-mode-hook
  (lambda ()
    ;; Highlight bad writing style.
    (writegood-mode 1)))

(provide 'conf/mode-specific/text)
