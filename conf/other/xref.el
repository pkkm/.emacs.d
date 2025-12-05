;;; Xref - a built-in unified interface for finding definitions/references. -*- lexical-binding: t -*-

(bind-key "C-c ." #'xref-find-definitions) ; Go to definition. Default: M-.
(bind-key "C-c ," #'xref-go-back) ; Go back. The default M-, binding isn't shadowed, but we add this one for consistency.
(bind-key "C-c ?" #'xref-find-references) ; Find references. Default: M-?

;; When there are multiple candidates, use completion instead of a separate window you have to click in.
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'conf/other/xref)
