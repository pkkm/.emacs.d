;;; Smartparens -- a modern alternative to paredit.

(require 'conf/packages)
(package-ensure-installed 'smartparens)

(smartparens-global-mode 1)

;; Modes where smartparens mode is inactive.
(setq sp-ignore-modes-list '()) ; Add minibuffer-inactive-mode to disable SP in the minibuffer.

;;; Highlighting.

;; Highlight matching pairs (like show-paren-mode, but with user-defined pairs).
(show-smartparens-global-mode 1)
(setq sp-show-pair-from-inside t) ; Point can be on any side of the delimiter.
(setq sp-show-pair-delay 0.1)
(face-spec-reset-face 'sp-show-pair-enclosing)
(setq blink-matching-paren nil) ; Don't move the cursor to the matching paren.

;; Highlighting.
(setq sp-highlight-pair-overlay nil) ; Don't highlight the currently edited expression.
(setq sp-highlight-wrap-overlay t) ; ???.
(setq sp-highlight-wrap-tag-overlay t) ; ???.

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
(setq sp-autoskip-closing-pair t)
(setq sp-cancel-autoskip-on-backward-movement nil)

;; Don't put an "undo-boundary" before each inserted pair.
(setq sp-undo-pairs-separately nil)

;;; Definition of sexp.

;; What to consider a sexp.
(setq sp-navigate-comments-as-sexps t) ; Consider comments to be sexps.
(setq sp-navigate-consider-symbols t) ; Consider symbols outside balanced expressions to be sexps.
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
(setq sp-navigate-close-if-unbalanced nil) ; Don't insert closing delimiter of unmatched pairs.
(setq sp-navigate-reindent-after-up '((interactive emacs-lisp-mode))) ; In which modes sexps should be reindented. With 'interactive, the sexp will be reindented only if `sp-up-sexp' was called interactively. With 'always, always.

;; Include some mode-specific pairs (for Lisp, LaTeX and HTML).
(require 'smartparens-config)

;;; Bindings.

(define-key sp-keymap (kbd "C-M-f") #'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") #'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") #'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") #'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") #'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") #'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") #'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") #'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") #'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") #'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") #'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") #'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") #'sp-copy-sexp)

;;(define-key sp-keymap (kbd "M-<delete>") #'sp-unwrap-sexp)
;;(define-key sp-keymap (kbd "M-<backspace>") #'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") #'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") #'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") #'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") #'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") #'sp-splice-sexp)
;;(define-key sp-keymap (kbd "C-M-<delete>") #'sp-splice-sexp-killing-forward)
;;(define-key sp-keymap (kbd "C-M-<backspace>") #'sp-splice-sexp-killing-backward)
;;(define-key sp-keymap (kbd "C-S-<backspace>") #'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") #'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") #'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") #'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") #'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") #'sp-backward-symbol)

;; TODO see how useful `sp-newline' will be with evil.

;;; Other bindings.

(defvar my-smartparens-map (make-sparse-keymap)
  "Keymap for rarely used Smartparens commands.")
(define-prefix-command 'my-smartparens-map)
(define-key evil-normal-state-map (kbd "g p") #'my-smartparens-map)

(define-key my-smartparens-map (kbd "t") #'sp-prefix-tag-object) ; Perform the next operation on an SGML tag.
(define-key my-smartparens-map (kbd "p") #'sp-prefix-pair-object) ; Perform the next operation on a balanced pair.

;; Splice sexp, killing backward. Then wrap the enclosing sexp with the killed one.
;; Example (| -- cursor):
;;  (let ((stuff 1))            |(while (we-are-good)
;;    (while (we-are-good)  ->     (let ((stuff 1))
;;     |(do-thing 1)                 (do-thing 1)
;;      (do-thing 2)))               (do-thing 2)))
(define-key my-smartparens-map (kbd "c") #'sp-convolute-sexp)

;; Absorb -- move the sexp before the one we're in into it, at the cursor position.
;; Emit -- the reverse.
;; Example (| -- cursor):
;;  (do-stuff 1)                  (save-excursion
;;  (save-excursion  --absorb-->   |(do-stuff 1)
;;   |(do-stuff 2))                 (do-stuff 2))
(define-key my-smartparens-map (kbd "a") #'sp-absorb-sexp)
(define-key my-smartparens-map (kbd "e") #'sp-emit-sexp)
(define-key my-smartparens-map (kbd "A") #'sp-emit-sexp)
(define-key my-smartparens-map (kbd "E") #'sp-absorb-sexp)

;; Add the expression after/before point to the list before/after point (like slurp forward/backward, but from the outside).
(define-key my-smartparens-map (kbd "p") #'sp-add-to-previous-sexp)
(define-key my-smartparens-map (kbd "n") #'sp-add-to-next-sexp)

;; Join, split.
(define-key my-smartparens-map (kbd "j") #'sp-join-sexp)
(define-key my-smartparens-map (kbd "T") #'sp-join-sexp)
(define-key my-smartparens-map (kbd "s") #'sp-split-sexp)

(provide 'conf/editing/smartparens)
