;;; Pascal.

(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

(add-one-shot-hook
 'pascal-mode-hook
 (lambda ()
   ;; Indent with a TAB.
   (defvaralias 'pascal-indent-level 'tab-width)
   (defvaralias 'pascal-case-indent 'tab-width) ; Case statements.

   ;; Don't automatically line up characters.
   (setq pascal-auto-lineup '())

   ;; Electric characters:
   ;; # -- unindent if this is a CPP directive.
   ;; = -- align if containing statement is in `pascal-auto-lineup'.
   ;; : -- align if containing statement is in `pascal-auto-lineup'.
   ;; ; -- autoindent.
   ;; . -- autoindent.

   ;; C-c:
   ;;   C-o -- outline mode.
   ;;   C-d -- goto defun.
   ;;   C-c -- comment area.
   ;;   C-u -- uncomment area.
   ;;   C-b -- insert block.

   ;; Disable DEL -- delete and convert the deleted tabs to spaces.
   (define-key pascal-mode-map (kbd "DEL") nil)))

(provide 'conf/lang/pascal)
