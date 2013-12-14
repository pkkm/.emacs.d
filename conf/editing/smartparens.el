;;; Smartparens -- a modern alternative to paredit.

(require 'conf/utils/keys) ; Used: evil-define-key-in-states.

(require 'conf/packages)
(package-ensure-installed 'smartparens)

(smartparens-global-mode 1)

;; Don't show Smartparens in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'smartparens-mode "")

;; Modes where smartparens mode is inactive.
(setq sp-ignore-modes-list '()) ; Add minibuffer-inactive-mode to disable SP in the minibuffer.

;;; Highlighting.

;; Highlight matching pairs (like show-paren-mode, but with user-defined pairs).
(show-smartparens-global-mode 1)
(setq sp-show-pair-from-inside t) ; Point can be on any side of the delimiter.
(setq sp-show-pair-delay 0.1)
(face-spec-reset-face 'sp-show-pair-enclosing)
(setq blink-matching-paren nil) ; Don't move the cursor to the matching paren.

;; Don't underline the currently edited expression.
(setq sp-highlight-pair-overlay nil)

;;; Autoinsert and autoskip.

;; Autoinsert pairs (even when the point is followed by a word).
(setq sp-autoinsert-pair t)
(setq sp-autoinsert-if-followed-by-word t)

;; If the opening and closing delimiter are the same and the enclosed expression is empty, nest the pairs (useful in modes where repeated characters are used as delimiters, for example "**text**" in Markdown). If the enclosed expression is not empty, skip the closing delimiter.
;; TODO (FIXME) this break smartparens:
;;(setq sp-autoinsert-if-followed-by-same 4)

;; Disable special behavior for strings.
(setq sp-autoescape-string-quote nil)
(setq sp-autoinsert-quote-if-followed-by-closing-pair t)

;; Skip closing pair instead of inserting it.
(setq sp-autoskip-closing-pair 'always)
(setq sp-cancel-autoskip-on-backward-movement nil)

;; Don't put an "undo-boundary" before each inserted pair.
(setq sp-undo-pairs-separately nil)

;;; Definition of sexp.

;; What to consider a sexp.
(setq sp-navigate-comments-as-sexps t) ; Consider comments to be sexps.
(setq sp-navigate-consider-symbols t) ; Consider symbols outside balanced expressions to be sexps. ; WARNING! SETTING THIS TO NIL CAUSES SEXP DELETION ON BACKWARD-UP-SEXP! ; TODO check if warning still applies, after a few days.
(setq sp-navigate-consider-sgml-tags ; In which modes to consider SGML tags to be sexps.
      '(sgml-mode html-mode xml-mode nxml-mode scala-mode))
(setq sp-navigate-consider-stringlike-sexp '(latex-mode)) ; In which modes to consider string-like sexps (like "*bold text*") to be sexps.

;; Functions to skip over paired expression matches in given major modes.
;; Use this for things that can both be and not be expression delimiters, for example "*" in Markdown, which can signify a list item or emphasis. If the exception is only relevant to one pair, use the :skip-match option in `sp-local-pair'.
;;(setq sp-navigate-skip-match
      ;;'(((emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode scheme-mode lisp-mode eshell-mode slime-repl-mode nrepl-mode clojure-mode common-lisp-mode)
         ;;. sp--elisp-skip-match)))

;;; Other.

;; Wrapping.
(setq sp-autowrap-region t) ; When pair inserted with region, wrap the region.
(setq sp-wrap-entire-symbol 0) ; Wrap the entire symbol instead of only the part after point. 0 -- enable globally, 1 -- disable globally, list of major modes -- disable in major modes.

;; If a character from "()" is deleted, delete the whole pair.
(setq sp-autodelete-closing-pair t)
(setq sp-autodelete-opening-pair t)
(setq sp-autodelete-pair t)
(setq sp-autodelete-wrap nil) ; No special behavior for most recent wrapping.

;; Sexp cleanups done on `sp-up-sexp'.
(setq sp-navigate-close-if-unbalanced nil) ; Don't insert the closing delimiter of unmatched pairs.
(setq sp-navigate-reindent-after-up ; In which modes sexps should be reindented.
      '((interactive emacs-lisp-mode lisp-interaction-mode))) ;; With 'interactive, reindent only if called interactively. With 'always, always.

;; Include some mode-specific pairs (for Lisp, LaTeX and HTML).
(require 'smartparens-config)

;;; Bindings.

;; Unbind Evil's [ and ] prefixes (movement by sections is useless anyway).
(require 'conf/evil)
(dolist (keymap (list evil-motion-state-map
                      evil-normal-state-map))
  (define-key keymap (kbd "[") nil)
  (define-key keymap (kbd "]") nil))

;; Unbind Evil's C-y (motion state: scroll up, insert: copy from above).
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)

;; "g p" will be used as a prefix for uncommon Smartparens commands.

;; Prefix arguments.
(evil-define-key 'motion sp-keymap (kbd "g >") #'sp-prefix-tag-object) ; Perform the next operation on an SGML tag.
(evil-define-key 'motion sp-keymap (kbd "g \"") #'sp-prefix-pair-object) ; Perform the next operation on a balanced pair. (Use this to skip symbols when moving by sexps.)

;; Usual meaning of raw prefix arguments to Smartparens commands:
;;   single prefix -- repeat as many times as possible.
;;   double prefix -- operate on the enclosing expression.

;; Move by sexps, cursor at the beginning (like w/b).
(evil-define-key 'motion sp-keymap (kbd "C-s") #'sp-next-sexp)
(evil-define-key 'motion sp-keymap (kbd "C-y") #'sp-backward-sexp)

;; Other movement by sexps (cursor at the end, like e/ge).
;;(define-key sp-keymap (kbd "C-M-f") #'sp-forward-sexp)
;;(define-key sp-keymap (kbd "C-M-p") #'sp-previous-sexp)

;; Move up/down nested sexps (cursor at the beginning).
(define-key sp-keymap (kbd "C-)") #'sp-down-sexp)
(define-key sp-keymap (kbd "C-(") #'sp-backward-up-sexp)
(when (eq window-system 'w32) ; C-( and C-) are incorrectly interpreted with my Portable Keyboard Layout config.
  (define-key sp-keymap (kbd "C-2") #'sp-down-sexp)
  (define-key sp-keymap (kbd "C-1") #'sp-backward-up-sexp))

;; Other movement up/down nested sexps (cursor at the end).
;;(define-key sp-keymap (kbd "C-M-a") #'sp-backward-down-sexp)
;;(define-key sp-keymap (kbd "C-M-e") #'sp-up-sexp)

;; Beginning/end of sexp.
;; With non-numeric prefix, beginning/end of enclosing sexp.
;; With prefix ARG, beginning/end of ARGth next sexp (ARG can be negative).
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

;; Not needed due to the operator/motion nature of evil and text objects.
;;(define-key sp-keymap (kbd "C-M-k") #'sp-kill-sexp)
;;(define-key sp-keymap (kbd "C-M-w") #'sp-copy-sexp)
;;(define-key sp-keymap (kbd "C-]") #'sp-select-next-thing-exchange)
;;(define-key sp-keymap (kbd "C-<left_bracket>") #'sp-select-previous-thing)
;;(define-key sp-keymap (kbd "C-M-]") #'sp-select-next-thing)

;; TODO see how useful `sp-newline' will be with evil.

;; Normalize keymaps first time Smartparens mode is activated.
;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
;; See <https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not>.
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.
(add-one-shot-hook 'smartparens-mode-hook #'evil-normalize-keymaps)

(provide 'conf/editing/smartparens)
