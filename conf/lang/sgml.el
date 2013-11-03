;;; SGML (parent of HTML).

;; Indentation (Smart Tabs).
(smart-tabs-add-language-support sgml sgml-mode-hook
  ((sgml-indent-line . sgml-basic-offset)))
(smart-tabs-insinuate 'sgml)
(add-hook 'sgml-mode-hook (lambda () (setq indent-tabs-mode 1)))

;; Emmet (formerly Zen Coding) -- expand abbreviations.
;; Default binding: C-j
(require 'conf/other/emmet)
(add-hook 'sgml-mode-hook (lambda () (emmet-mode 1)))

(provide 'conf/lang/sgml)
