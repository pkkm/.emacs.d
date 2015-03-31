;;; Pascal.

(use-package pascal ; Bundled with Emacs.
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support pascal pascal-mode-hook
      ((pascal-indent-line . pascal-indent-level)))
    (smart-tabs-insinuate 'pascal)
    (add-hook 'pascal-mode-hook #'enable-indent-tabs-mode))

  ;; Indent case statements same as everything else.
  (defvaralias 'pascal-case-indent 'pascal-indent-level)

  ;; Electric characters:
  ;; # -- unindent if this is a CPP directive.
  ;; ; . -- autoindent.
  (setq pascal-auto-lineup '()) ; Don't automatically align characters.
  ;; = : -- align if containing statement is in `pascal-auto-lineup'.

  ;; C-c:
  ;;   C-o -- outline mode.
  ;;   C-d -- goto defun.
  ;;   C-b -- insert block.
  ;;   C-c, C-u -- comment/uncomment area.

  ;; Don't convert tabs to spaces when pressing backspace.
  (bind-key "DEL" nil pascal-mode-map))

(provide 'conf/mode-specific/pascal)
