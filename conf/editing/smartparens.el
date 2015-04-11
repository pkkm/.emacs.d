;;; Smartparens -- a modern alternative to paredit.

(use-package smartparens
  :ensure smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode 1)
  :config

  ;; Some default settings (e.g. mode-specific pairs for Lisp, HTML, LaTeX).
  (require 'smartparens-config)

  ;; Highlight matching pairs (like show-paren-mode, but with user-defined pairs).
  (show-smartparens-global-mode 1)
  (setq sp-show-pair-from-inside t) ; Point can be on any side of the delimiter.
  (setq sp-show-pair-delay 0.1)
  (setq blink-matching-paren nil) ; Don't move the cursor to the matching paren.

  ;; Don't underline the currently edited expression.
  (setq sp-highlight-pair-overlay nil)

  ;; In which modes to consider SGML tags to be sexps.
  (setq sp-navigate-consider-sgml-tags
        (append sp-navigate-consider-sgml-tags
                '(sgml-mode xml-mode nxml-mode scala-mode)))

  ;; For some keys, use commands that operate either on sexps or hybrid-sexps, depending on mode.
  ;; TODO feature request for similar functionality in Smartparens?
  (use-package dash :ensure dash :commands -any-p)
  (require 'conf/utils/modes) ; Used: derived-mode-hierarchy.
  (defmacro my-sp-maybe-hybrid-sexp-command (normal-command hybrid-sexp-command)
    "Define a command named my-NORMAL-COMMAND that calls NORMAL-COMMAND or HYBRID-SEXP-COMMAND."
    `(defun ,(intern (concat "my-" (symbol-name normal-command))) ()
       ,(concat "Call `" (symbol-name hybrid-sexp-command) "'"
                " if the current mode (or one of its parents) is in `my-sp-hybrid-sexp-modes', "
                "`" (symbol-name normal-command) "' otherwise.")
       (interactive)
       (if (-any-p (lambda (mode) (memq mode my-sp-hybrid-sexp-modes))
                   (derived-mode-hierarchy major-mode))
           (call-interactively #',hybrid-sexp-command)
         (call-interactively #',normal-command))))
  (defvar my-sp-hybrid-sexp-modes '(cc-mode ruby-mode java-mode scala-mode)
    "Modes in which to use hybrid sexp operations instead of normal ones.")

  ;; Keybindings.
  ;; Nice non-Smartparens binding: M-) -- insert a new sexp after the one we're in.
  (with-eval-after-load 'evil
    ;; Prefix arguments.
    (evil-define-key 'motion sp-keymap (kbd "g >") #'sp-prefix-tag-object) ; Perform the next operation on an SGML tag.
    (evil-define-key 'motion sp-keymap (kbd "g )") #'sp-prefix-pair-object) ; Perform the next operation on a balanced pair. (Use this to skip symbols when moving by sexps.)

    ;; Usual meaning of raw prefix arguments to Smartparens commands:
    ;;   single prefix -- repeat as many times as possible.
    ;;   double prefix -- operate on the enclosing expression.

    ;; Move by sexps, cursor at the beginning (like w/b).
    (bind-key "C-y" nil evil-motion-state-map) ; Deleted binding: scroll up.
    (bind-key "C-y" nil evil-insert-state-map) ; Deleted binding: copy from above.
    (evil-define-key 'motion sp-keymap (kbd "C-s") #'sp-next-sexp)
    (evil-define-key 'motion sp-keymap (kbd "C-y") #'sp-backward-sexp)

    ;; Move inside/outside nested sexps.
    (bind-key "C-)" #'sp-down-sexp sp-keymap)
    (bind-key "C-(" #'sp-backward-up-sexp sp-keymap)
    (when (not (display-graphic-p)) ; Versions with the Meta key, for terminals which don't support the above characters.
      (bind-key "M-)" #'sp-down-sexp sp-keymap)
      (bind-key "M-(" #'sp-backward-up-sexp sp-keymap))

    ;; Beginning/end of sexp.
    ;; With non-numeric prefix, beginning/end of enclosing sexp.
    ;; With prefix ARG, beginning/end of ARGth next sexp (ARG can be negative).
    (dolist (keymap (list evil-motion-state-map evil-normal-state-map)) ; I didn't use movement by sections anyway.
      (bind-key "[" nil keymap)
      (bind-key "]" nil keymap))
    (evil-define-key 'motion sp-keymap (kbd "[") #'sp-beginning-of-sexp)
    (evil-define-key 'motion sp-keymap (kbd "]") #'sp-end-of-sexp)

    ;; Delete to the end of sexp.
    (evil-define-key 'motion sp-keymap (kbd "M-d") #'sp-kill-hybrid-sexp)

    ;; Slurp/barf forward.
    ;;   Slurp -- move the next sexp inside the one we're in.
    ;;   Barf -- push out the last element of the sexp we're in.
    ;; With non-numeric prefix, slurp/barf as many as possible.
    (my-sp-maybe-hybrid-sexp-command sp-forward-slurp-sexp sp-slurp-hybrid-sexp) ; There is no backward hybrid-sexp variant.
    (bind-key "C-}" #'my-sp-forward-slurp-sexp sp-keymap)
    (bind-key "C-{" #'sp-forward-barf-sexp sp-keymap)
    (when (not (display-graphic-p)) ; Versions with the Meta key, for terminals which don't support the above characters.
      (bind-key "M-}" #'my-sp-forward-slurp-sexp sp-keymap)
      (bind-key "M-{" #'sp-forward-barf-sexp sp-keymap))
    ;; Slurp/barf backward -- operate on the sexp before the one we're in.
    (evil-define-key 'normal sp-keymap (kbd "g {") #'sp-backward-slurp-sexp)
    (evil-define-key 'normal sp-keymap (kbd "g }") #'sp-backward-barf-sexp)

    ;; Absorb -- move the previous sexp inside the one we're in, at point.
    ;; Example (| -- cursor):
    ;;  (do-stuff 1)           (save-excursion
    ;;  (save-excursion   ->    |(do-stuff 1)
    ;;   |(do-stuff 2))          (do-stuff 2))
    (evil-define-key 'normal sp-keymap (kbd "g p {") #'sp-absorb-sexp)

    ;; Convolute -- splice sexp, killing backward. Then wrap the enclosing sexp with the killed one.
    ;; With prefix ARG, move that many sexps up before wrapping.
    ;; Example (| -- cursor):
    ;;  (let ((stuff 1))              |(while (we-are-good)
    ;;    (while (we-are-good)   ->      (let ((stuff 1))
    ;;     |(do-thing 1)                   (do-thing 1)
    ;;      (do-thing 2)))                 (do-thing 2)))
    (evil-define-key 'normal sp-keymap (kbd "g p c") #'sp-convolute-sexp)

    ;; Add the sexp after point to the one before point, or the other way around.
    (evil-define-key 'normal sp-keymap (kbd "g p p") #'sp-add-to-previous-sexp) ; Add the next sexp to the previous one.
    (evil-define-key 'normal sp-keymap (kbd "g p n") #'sp-add-to-next-sexp) ; Add the previous sexp to the next one.

    ;; Transpose sexps -- swap the next with the previous.
    ;; With prefix ARG, drag the sexp before point that many sexps forward (ARG can be negative).
    (my-sp-maybe-hybrid-sexp-command sp-transpose-sexp sp-transpose-hybrid-sexp)
    (evil-define-key 'normal sp-keymap (kbd "g p t") #'my-sp-transpose-sexp)

    ;; Splice (remove the delimiters of current sexp).
    ;; With numeric argument, splice the sexp that many levels up.
    (evil-define-key 'normal sp-keymap (kbd "g s") #'sp-splice-sexp)
    ;; Splice with killing. Useful for deleting conditionals while preserving some sexps inside.
    (evil-define-key 'normal sp-keymap (kbd "g DEL") #'sp-splice-sexp-killing-backward)
    (evil-define-key 'normal sp-keymap (kbd "g p DEL") #'sp-splice-sexp-killing-around)

    ;; Join, split.
    (evil-define-key 'normal sp-keymap (kbd "g p s") #'sp-split-sexp) ; With non-numeric prefix, split all the sexps in current one into separate sexps.
    (evil-define-key 'normal sp-keymap (kbd "g p T") #'sp-join-sexp) ; With prefix ARG, join with that many following expressions (ARG can be negative). ("T" is for consistency with my binding for "join line".)

    (evil-define-text-object evil-sp-a-sexp (count &rest other-args)
      "Text object for the enclosing sexp. With COUNT, use the COUNTth sexp up."
      (sp-get (sp-get-enclosing-sexp count) (list :beg :end)))
    (bind-key "e" #'evil-sp-a-sexp evil-outer-text-objects-map)

    (evil-define-text-object evil-sp-inner-sexp (count &rest other-args)
      "Text object for the enclosing sexp, without delimiters. With COUNT, use the COUNTth sexp up."
      (sp-get (sp-get-enclosing-sexp count) (list :beg-in :end-in)))
    (bind-key "e" #'evil-sp-inner-sexp evil-inner-text-objects-map)

    ;; Normalize keymaps.
    ;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
    ;; See <https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not>.
    (evil-normalize-keymaps)))

(provide 'conf/editing/smartparens)
