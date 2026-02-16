;;; Xref - a built-in unified interface for finding definitions/references. -*- lexical-binding: t -*-

(bind-key "C-c ." #'xref-find-definitions) ; Go to definition. Default: M-.
(bind-key "C-c ," #'xref-go-back) ; Go back. The default M-, binding isn't shadowed, but we add this one for consistency.
(bind-key "C-c ?" #'xref-find-references) ; Find references. Default: M-?

;; Try also: `xref-find-references-and-replace' for renaming an identifier without LSP.

;; When there are multiple definitions, use completion instead of a separate window you have to click in.
;; See also: `xref-show-xrefs-function' if you want to change more than just go-to-definition.
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'conf/other/xref)
