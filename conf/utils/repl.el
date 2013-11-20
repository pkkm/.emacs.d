;;; Utilities for handling REPLs.

(defun repl-in-term (name program &optional isend-associate)
  "Execute PROGRAM in `term', in another window. Name the buffer *NAME*.
The terminal will be in line mode: edit a line at a time and then send it with `term-send-input' (RET).

If ISEND-ASSOCIATE is non-nil, enable sending the contents of the current buffer to the REPL with `isend-send' (M-RET).
Otherwise, if `isend-mode' is installed, display a message describing how to use it.

Command history:
  * `term-previous-input' (M-n)
  * `term-next-input' (C-c C-n)
  * `term-dynamic-list-input-ring' (C-c C-l)"
  (let ((term-buffer (make-term name program)))
    (with-current-buffer term-buffer
      (term-line-mode)
      (evil-insert-state))
    (display-buffer term-buffer)
    (when (package-installed-p 'isend-mode)
      (if isend-associate
          (progn
            (isend-associate (buffer-name term-buffer))
            (message "Use M-RET (`isend-send') in this buffer send a line or region to the REPL."))
        (message (concat "Use \"M-x isend-associate RET " (buffer-name term-buffer) " RET\" "
                         "and then M-RET (`isend-send') to send a line or region to this REPL."))))))

(provide 'conf/utils/repl)
