;;; Pascal.

(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

;; Indentation (Smart Tabs).
(smart-tabs-add-language-support pascal pascal-mode-hook
  ((pascal-indent-line . pascal-indent-level)))
(smart-tabs-insinuate 'pascal)
(add-hook 'pascal-mode-hook (lambda () (setq indent-tabs-mode 1)))
(defvaralias 'pascal-case-indent 'pascal-indent-level) ; Indent case statements same as everything else.

(add-one-shot-hook
 'pascal-mode-hook
 (lambda ()
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
