;;; Smartparens -- a modern alternative to paredit.

(require 'conf/packages)
(require 'conf/evil)

(use-package smartparens-config ; Use smartparens with the default configuration (which includes some mode-specific pairs for HTML, LaTeX, etc.).
  :ensure smartparens
  :diminish smartparens-mode
  :commands smartparens-global-mode
  :init
  (smartparens-global-mode 1)
  :config

  ;; Highlight matching pairs (like show-paren-mode, but with user-defined pairs).
  (show-smartparens-global-mode 1)
  (setq sp-show-pair-from-inside t) ; Point can be on any side of the delimiter.
  (setq sp-show-pair-delay 0.1)
  (setq blink-matching-paren nil) ; Don't move the cursor to the matching paren.

  ;; Don't underline the currently edited expression.
  (setq sp-highlight-pair-overlay nil)

  ;; Skip closing pair instead of inserting it.
  ;; When the closing delimiter is typed inside a sexp, place the point after the sexp end.
  (setq-default sp-autoskip-closing-pair 'always)

  ;; What to consider a sexp.
  (setq sp-navigate-consider-sgml-tags ; In which modes to consider SGML tags to be sexps.
        (append sp-navigate-consider-sgml-tags
                '(sgml-mode xml-mode nxml-mode scala-mode)))
  (setq sp-navigate-consider-stringlike-sexp ; In which modes to consider string-like sexps (like "*bold text*") to be sexps.
        (append sp-navigate-consider-stringlike-sexp
                '(latex-mode)))


  ;;; Bindings.

  ;; "g p" will be used as a prefix for uncommon Smartparens commands.

  ;; Prefix arguments.
  (evil-define-key 'motion sp-keymap (kbd "g >") #'sp-prefix-tag-object) ; Perform the next operation on an SGML tag.
  (evil-define-key 'motion sp-keymap (kbd "g \"") #'sp-prefix-pair-object) ; Perform the next operation on a balanced pair. (Use this to skip symbols when moving by sexps.)

  ;; Usual meaning of raw prefix arguments to Smartparens commands:
  ;;   single prefix -- repeat as many times as possible.
  ;;   double prefix -- operate on the enclosing expression.

  ;; Move by sexps, cursor at the beginning (like w/b).
  (define-key evil-motion-state-map (kbd "C-y") nil) ; Deleted binding: scroll up.
  (define-key evil-insert-state-map (kbd "C-y") nil) ; Deleted binding: copy from above.
  (evil-define-key 'motion sp-keymap (kbd "C-s") #'sp-next-sexp)
  (evil-define-key 'motion sp-keymap (kbd "C-y") #'sp-backward-sexp)

  ;; Other movement by sexps (cursor at the end, like e/ge).
  ;;(define-key sp-keymap (kbd "C-M-f") #'sp-forward-sexp)
  ;;(define-key sp-keymap (kbd "C-M-p") #'sp-previous-sexp)

  ;; Move up/down nested sexps (cursor at the beginning).
  (define-key sp-keymap (kbd "C-)") #'sp-down-sexp)
  (define-key sp-keymap (kbd "C-(") #'sp-backward-up-sexp)
  (when (not (display-graphic-p)) ; Versions with the Meta key, for terminals which don't support the above characters.
    (define-key sp-keymap (kbd "M-)") #'sp-down-sexp)
    (define-key sp-keymap (kbd "M-(") #'sp-backward-up-sexp))
  (when (eq window-system 'w32) ; C-( and C-) are incorrectly interpreted with my Portable Keyboard Layout config.
    (define-key sp-keymap (kbd "C-2") #'sp-down-sexp)
    (define-key sp-keymap (kbd "C-1") #'sp-backward-up-sexp))

  ;; Other movement up/down nested sexps (cursor at the end).
  ;;(define-key sp-keymap (kbd "C-M-a") #'sp-backward-down-sexp)
  ;;(define-key sp-keymap (kbd "C-M-e") #'sp-up-sexp)

  ;; Beginning/end of sexp.
  ;; With non-numeric prefix, beginning/end of enclosing sexp.
  ;; With prefix ARG, beginning/end of ARGth next sexp (ARG can be negative).
  (dolist (keymap (list evil-motion-state-map evil-normal-state-map)) ; I didn't use movement by sections anyway.
    (define-key keymap (kbd "[") nil)
    (define-key keymap (kbd "]") nil))
  (evil-define-key 'motion sp-keymap (kbd "[") #'sp-beginning-of-sexp)
  (evil-define-key 'motion sp-keymap (kbd "]") #'sp-end-of-sexp)

  ;;(define-key sp-keymap (kbd "M-F") #'sp-forward-symbol)
  ;;(define-key sp-keymap (kbd "M-B") #'sp-backward-symbol)

  ;; Delete to the end of sexp.
  (evil-define-key 'motion sp-keymap (kbd "M-d") #'sp-kill-hybrid-sexp)

  ;; Slurp and barf (move the next/previous expression inside/outside the current one).
  ;; With non-numeric prefix, slurp/barf as many as possible.
  (define-key sp-keymap (kbd "C-}") #'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-{") #'sp-forward-barf-sexp)
  (when (not (display-graphic-p)) ; Versions with the Meta key, for terminals which don't support the above characters.
    (define-key sp-keymap (kbd "M-}") #'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "M-{") #'sp-forward-barf-sexp))
  (when (eq window-system 'w32) ; C-( and C-) are incorrectly interpreted with my Portable Keyboard Layout config.
    (define-key sp-keymap (kbd "C-3") #'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-5") #'sp-forward-barf-sexp))
  (evil-define-key 'normal sp-keymap (kbd "g {") #'sp-backward-slurp-sexp)
  (evil-define-key 'normal sp-keymap (kbd "g }") #'sp-backward-barf-sexp)

  ;; Absorb -- move the sexp before the one we're in into it, at the cursor position.
  ;; Emit -- the reverse.
  ;; Example (| -- cursor):
  ;;  (do-stuff 1)                  (save-excursion
  ;;  (save-excursion  --absorb-->   |(do-stuff 1)
  ;;   |(do-stuff 2))                 (do-stuff 2))
  (evil-define-key 'normal sp-keymap (kbd "g p {") #'sp-absorb-sexp)
  (evil-define-key 'normal sp-keymap (kbd "g p }") #'sp-emit-sexp)

  ;; Convolute -- splice sexp, killing backward. Then wrap the enclosing sexp with the killed one.
  ;; With prefix ARG, move that many sexps up before wrapping.
  ;; Example (| -- cursor):
  ;;  (let ((stuff 1))            |(while (we-are-good)
  ;;    (while (we-are-good)  ->     (let ((stuff 1))
  ;;     |(do-thing 1)                 (do-thing 1)
  ;;      (do-thing 2)))               (do-thing 2)))
  (evil-define-key 'normal sp-keymap (kbd "g p c") #'sp-convolute-sexp)

  ;; Add the expression after/before point to the list before/after point (like slurp forward/backward, but from the outside).
  (evil-define-key 'normal sp-keymap (kbd "g p p") #'sp-add-to-previous-sexp)
  (evil-define-key 'normal sp-keymap (kbd "g p n") #'sp-add-to-next-sexp)

  ;; Transpose sexps -- swap the next with the previous.
  ;; With prefix ARG, drag the sexp before point that many sexps forward (ARG can be negative).
  (evil-define-key 'normal sp-keymap (kbd "g p t") #'sp-transpose-sexp)

  ;; Splice (remove the delimiters of enclosing sexp).
  ;; With prefix ARG, splice the sexp that many levels up.
  (evil-define-key 'normal sp-keymap (kbd "g s") #'sp-splice-sexp)
  (evil-define-key 'normal sp-keymap (kbd "g DEL") #'sp-splice-sexp-killing-backward)
  (evil-define-key 'normal sp-keymap (kbd "g M-DEL") #'sp-splice-sexp-killing-forward)
  (evil-define-key 'normal sp-keymap (kbd "g p DEL") #'sp-splice-sexp-killing-around)

  ;; Unwrap (remove the delimiters of previous/next sexp).
  ;;(define-key sp-keymap (kbd "M-<delete>") #'sp-unwrap-sexp)
  ;;(define-key sp-keymap (kbd "M-<backspace>") #'sp-backward-unwrap-sexp)

  ;; Join, split.
  (evil-define-key 'normal sp-keymap (kbd "g p s") #'sp-split-sexp) ; With non-numeric prefix, split all the sexps in current one into separate sexps.
  (evil-define-key 'normal sp-keymap (kbd "g p j") #'sp-join-sexp) ; With prefix ARG, join with that many following expressions (ARG can be negative).
  (evil-define-key 'normal sp-keymap (kbd "g p T") #'sp-join-sexp) ; For consistency with my non-standard binding for "join line".

  (evil-define-text-object evil-sp-a-sexp (count &rest other-args)
    "Text object for the enclosing sexp. With COUNT, use the COUNTth sexp up."
    (sp-get (sp-get-enclosing-sexp count) (list :beg :end))) ; `sp-get-enclosing-sexp' can take ARG to return the expression that many times up.
  (define-key evil-outer-text-objects-map "e" 'evil-sp-a-sexp)

  (evil-define-text-object evil-sp-inner-sexp (count &rest other-args)
    "Text object for the enclosing sexp, without delimiters. With COUNT, use the COUNTth sexp up."
    (sp-get (sp-get-enclosing-sexp count) (list :beg-in :end-in))) ; `sp-get-enclosing-sexp' can take ARG to return the expression that many times up.
  (define-key evil-inner-text-objects-map "e" 'evil-sp-inner-sexp)

  ;; TODO equivalent of evil-surround.
  ;; TODO see how useful `sp-newline' will be with evil.

  ;; Normalize keymaps.
  ;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
  ;; See <https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not>.
  (evil-normalize-keymaps))

(provide 'conf/editing/smartparens)
