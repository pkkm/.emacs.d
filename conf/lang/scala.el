;;; Scala.

;; Scala mode 2 -- for Scala 2.9+.
(require 'conf/packages)
(package-ensure-installed 'scala-mode2)

(require 'conf/utils/hooks) ; Used: add-one-shot-hook.
(add-one-shot-hook
 'scala-mode-hook
 (lambda ()
   ;; Indentation: If `indent-tabs-mode' is non-nil, use one TAB, otherwise `tab-width' spaces.
   (defvaralias 'scala-indent:step 'tab-width)
   (kill-local-variable indent-tabs-mode)))

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
