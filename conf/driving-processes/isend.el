;;; Isend -- send parts of an Emacs buffer to a REPL.

(use-package isend-mode
  :ensure isend-mode
  :defer t
  :config

  ;; The key for sending the current line (or region) to the associated REPL.
  ;; Use "M-x isend-associate RET *repl-name* RET" to associate the current buffer with *repl-name*.
  (bind-key "C-RET" nil isend-mode-map)
  (bind-key "M-RET" #'isend-send isend-mode-map)

  ;; Delete indentation in sent regions (preserves indentation relative to the first line in region).
  (setq isend-delete-indentation t)

  ;; Don't append (one more) empty line to everything sent.
  (setq isend-end-with-empty-line nil))

(provide 'conf/driving-processes/isend)
