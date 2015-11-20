;;; Scala.

(use-package scala-mode2 ; Scala mode 2 -- for Scala 2.9+.
  :ensure t
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support scala scala-mode-hook
      ((scala-indent:indent-line . scala-indent:step)))
    (smart-tabs-insinuate 'scala)
    (add-hook 'scala-mode-hook #'enable-indent-tabs-mode)))

(provide 'conf/mode-specific/scala)
