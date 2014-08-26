;;; Scala.

(use-package scala-mode2 ; Scala mode 2 -- for Scala 2.9+.
  :ensure scala-mode2
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support scala scala-mode-hook
      ((scala-indent:indent-line . scala-indent:step)))
    (smart-tabs-insinuate 'scala)
    (add-hook 'scala-mode-hook #'enable-indent-tabs-mode))

  (require 'conf/utils/repl) ; Used: repl-in-term.
  (defun scala-repl ()
    "Launch a Scala REPL in another window.
If the current buffer is in Scala mode, `isend-associate' it with the REPL."
    (interactive)
    (repl-in-term "scala-repl" "scala" (eq major-mode 'scala-mode))))

(provide 'conf/mode-specific/scala)
