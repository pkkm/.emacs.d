;;; Isend -- send parts of an Emacs buffer to a REPL.

(use-package isend-mode
  :ensure t
  :defer t
  :init

  (defun isend-associated-ansi-term (program)
    "Start PROGRAM in `ansi-term', in a window chosen by `display-buffer' and `isend-associate' the current buffer with it.
The terminal will be in line mode: edit a line at a time and then send it with `term-send-input' (RET)."
    (interactive (list (read-from-minibuffer "Run program: ")))
    (let (term-buffer
          ;; `ansi-term' normally displays the terminal in the current window, but we want it in another, so we'll create a temporary buffer, display it and launch `ansi-term' there.
          (buffer-for-display (generate-new-buffer "*place-for-term*")))
      (with-selected-window (display-buffer buffer-for-display)
        (let ((term-ansi-buffer-base-name t)) ; Name the buffer after the program.
          (ansi-term program)) ; `ansi-term' always creates a new buffer (in contrast to `term', which reuses).
        (term-line-mode)
        (evil-insert-state)
        (kill-buffer buffer-for-display)
        (setq term-buffer (current-buffer)))
      (if (fboundp 'isend-associate)
          (progn
            (isend-associate (buffer-name term-buffer))
            (message "Use M-RET (`isend-send') to send a line or region to the REPL."))
        (message (concat "Use M-x isend-associate RET %s RET and then "
                         "M-RET (`isend-send') to send lines/regions to the REPL.")
                 (buffer-name term-buffer)))))

  :config

  ;; M-RET -- send the current line (or region) to the associated REPL.
  ;; Use "M-x isend-associate RET *repl-name* RET" to associate the current buffer with *repl-name*.
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap isend-mode-map)
  (bind-key "M-RET" #'isend-send isend-mode-map)

  ;; Delete indentation in sent regions (preserves indentation relative to the first line in region).
  (setq isend-delete-indentation t)

  ;; Don't append (one more) empty line to everything sent.
  (setq isend-end-with-empty-line nil))

(provide 'conf/driving-processes/isend)
