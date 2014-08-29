;;; Confirmations.

;; Prompt for "y" or "n" instead of "yes" or "no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask for confirmation when visiting a nonexistent file or buffer.
(with-eval-after-load 'ido
  (setq ido-create-new-buffer 'always))
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't ask for confirmation when killing a buffer with a running process.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(provide 'conf/minibuffer/confirmations)
