;;; Scala.

;; Scala mode 2 -- for Scala 2.9+.
(require 'conf/packages)
(package-ensure-installed 'scala-mode2)

;; Indentation (Smart Tabs).
(smart-tabs-add-language-support scala scala-mode-hook
  ((scala-indent:indent-line . scala-indent:step)))
(smart-tabs-insinuate 'scala)
(add-hook 'scala-mode-hook (lambda () (setq indent-tabs-mode 1)))

(defun scala-repl ()
  "Open the Scala REPL in `term'.
Edit a line at a time and then send it with RET (`term-send-input').
History: `term-{next,previous}-input' (C-c C-n, M-n), `term-dynamic-list-input-ring' (C-c C-l)."
  (interactive)
  (term "scala")
  (term-line-mode)
  (rename-buffer "*scala-repl*")
  (message (concat "Use \"M-x isend-associate RET " (buffer-name) " RET\" in a buffer to send parts of it to this REPL.")))

(provide 'conf/lang/scala)
