;;; Buffer utilities. -*- lexical-binding: t -*-

(defun buffers-opened-in-windows ()
  "A list of all buffers that are opened in a window."
  (let ((buffers (list)))
    (walk-windows (lambda (window)
                    (push (window-buffer window) buffers))
                  t t)
    buffers))

(provide 'conf/utils/buffers)
