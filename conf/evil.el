;;; Evil (Extensible VI Layer). -*- lexical-binding: t -*-

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil) ; Should make "primitive-undo: Unrecognized entry in undo list undo-tree-canary" errors less frequent.
  (setq undo-tree-auto-save-history nil)) ; Don't litter directories with `*.~undo-tree~' files.

(use-package evil
  :ensure t

  :preface
  (setq evil-want-Y-yank-to-eol t) ; Yank from cursor to end instead of whole line (for consistency with C and D).
  (setq evil-undo-system 'undo-tree)

  :init
  (evil-mode)

  :config

  (global-undo-tree-mode 1)

  ;; Search.
  (setq evil-regexp-search nil) ; Don't use regexes for / and ?.
  (setq evil-flash-delay 999999) ; Number of seconds to highlight matches for.

  (setq evil-start-of-line t) ; Make "gg" go to the start of first line instead of preserving cursor position. See <https://github.com/emacs-evil/evil/issues/1535>.
  (setq evil-cross-lines t) ; Allow "f" and similar motions to cross newlines.
  (setq evil-move-cursor-back nil) ; Don't move the cursor back when exiting insert state.
  (setq evil-move-beyond-eol t) ; Allow the cursor to be at the end of the line in normal state.
  (setq evil-echo-state nil) ; Don't echo state in the echo area (minibuffer).
  (setq evil-shift-round nil) ; When using < or >, don't round indentation to `evil-shift-width'.


  ;;; Tweak Vim bindings.

  ;; First non-blank/last character of the line: H/L (previously: first/last visible line in the window).
  (bind-key "H" #'evil-first-non-blank evil-motion-state-map)
  (bind-key "L" #'evil-end-of-line evil-motion-state-map)


  ;;; Unset Vim bindings I don't find useful.

  (bind-key "C-a" nil evil-insert-state-map)
  (bind-key "C-e" nil evil-insert-state-map)
  (bind-key "RET" nil evil-motion-state-map)

  ;; Manual indentation in insert state (I usually use automatic indentation).
  (bind-key "C-t" nil evil-insert-state-map)
  (bind-key "C-d" nil evil-insert-state-map)

  ;; Completion (I use another package for that).
  (bind-key "C-n" nil evil-insert-state-map)
  (bind-key "C-p" nil evil-insert-state-map)

  ;; Vim bindings in the ":" prompt (inconsistent with bindings in other prompts).
  (dolist (key '("C-a" "C-b" "C-c" "C-d" "C-k" "C-l" "C-r" "C-u" "C-v" "C-w"))
    (bind-key key nil evil-ex-completion-map))


  ;; Don't allow any keymap to shadow Evil bindings.
  (setq evil-overriding-maps '()) ; Keymaps above global state keymap, but below the local one.
  (setq evil-pending-overriding-maps '()) ; No idea what it's for, but setting only the above doesn't seem to work e.g. in compilation-mode.
  ;;(setq evil-intercept-maps '()) ; Keymaps above all others (useful when debugging).

  ;; Don't allow modes to change the definition of paragraph used by { and }.
  (defun kill-local-paragraph-definitions ()
    "Kill the local values of `paragraph-start' and `paragraph-separate'."
    (kill-local-variable 'paragraph-start)
    (kill-local-variable 'paragraph-separate))
  (add-hook 'after-change-major-mode-hook #'kill-local-paragraph-definitions)

  ;; Initial states for modes which don't have their own sections in my config.
  (evil-set-initial-state 'debugger-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'occur-mode 'motion)

  ;; Normalize keymaps after init.
  ;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
  ;; See <https://github.com/emacs-evil/evil/issues/301>.
  (add-hook 'after-init-hook #'evil-normalize-keymaps))

(provide 'conf/evil)
