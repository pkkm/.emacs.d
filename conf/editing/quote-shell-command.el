;;; A binding to quote a shell command in a major-mode-dependent way. -*- lexical-binding: t -*-

(defun my-quote-shell-command (beg end command)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) nil)
     (list nil nil (read-from-minibuffer "Command to insert: "))))
  (when beg
    (setq command (buffer-substring-no-properties beg end))
    (kill-region beg end))
  (let ((parts (split-string-shell-command command))
        (is-lisp (derived-mode-p 'lisp-mode 'lisp-data-mode 'clojure-mode)))
    (insert (mapconcat (lambda (str) (format "%S" str))
                       parts
                       (if is-lisp " " ", ")))))

(bind-key "C-c q" #'my-quote-shell-command)

(provide 'conf/editing/quote-shell-command)
