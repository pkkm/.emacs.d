;;; AsciiDoc.

(require 'conf/packages)
(package-ensure-installed 'adoc-mode)

;; Use for .asciidoc files.
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; Use a variable-pitch face.
(add-hook 'adoc-mode-hook (lambda ()
                            (variable-pitch-mode 1)))

;; Disable changing the definition of a paragraph.
(add-hook 'adoc-mode-hook (lambda ()
                            (kill-local-variable 'paragraph-separate)
                            (kill-local-variable 'paragraph-start)
                            (kill-local-variable 'paragraph-ignore-fill-prefix)))

;; For further customization, see M-x customize-group RET adoc.

(provide 'conf/lang/asciidoc)
