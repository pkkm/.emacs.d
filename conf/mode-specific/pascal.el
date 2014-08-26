;;; Pascal.

(require 'conf/editing/indentation)

(use-package pascal ; Bundled with Emacs.
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (smart-tabs-add-language-support pascal pascal-mode-hook
    ((pascal-indent-line . pascal-indent-level)))
  (defvaralias 'pascal-case-indent 'pascal-indent-level) ; Indent case statements same as everything else.
  (smart-tabs-insinuate 'pascal)
  (add-hook 'pascal-mode-hook #'enable-indent-tabs-mode)

  ;; Electric characters:
  ;; # -- unindent if this is a CPP directive.
  ;; ; -- autoindent.
  ;; . -- autoindent.
  (setq pascal-auto-lineup '()) ; Don't automatically align characters.
  ;; = -- align if containing statement is in `pascal-auto-lineup'.
  ;; : -- align if containing statement is in `pascal-auto-lineup'.

  ;; C-c:
  ;;   C-o -- outline mode.
  ;;   C-d -- goto defun.
  ;;   C-b -- insert block.
  ;;   C-c, C-u -- comment/uncomment area.

  ;; Don't convert tabs to spaces when pressing backspace.
  (define-key pascal-mode-map (kbd "DEL") nil))

(provide 'conf/mode-specific/pascal)
