;;; SGML (parent of HTML).

(use-package sgml-mode ; Bundled with Emacs.
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support sgml sgml-mode-hook
      ((sgml-indent-line . sgml-basic-offset)))
    (smart-tabs-insinuate 'sgml)
    (add-hook 'sgml-mode-hook #'enable-indent-tabs-mode))

  ;; When editing an opening tag, automatically update the closing tag.
  (add-hook 'sgml-mode-hook #'sgml-electric-tag-pair-mode)

  ;; Expand abbreviations like "p>ul>li*5" with C-j.
  (require 'conf/editing/emmet)
  (add-hook 'sgml-mode-hook #'emmet-mode))

(provide 'conf/mode-specific/sgml)
