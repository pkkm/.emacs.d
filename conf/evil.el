;;; Evil (Extensible VI Layer). -*- lexical-binding: t -*-

(require 'conf/editing/undo) ; Make sure that undo-tree is ready.

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
  (evil-define-key 'motion 'global (kbd "H") #'evil-first-non-blank)
  (evil-define-key 'motion 'global (kbd "L") #'evil-end-of-line)


  ;;; Unset Vim bindings I don't find useful.

  (evil-define-key 'insert 'global (kbd "C-a") nil)
  (evil-define-key 'insert 'global (kbd "C-e") nil)
  (evil-define-key 'motion 'global (kbd "RET") nil)

  ;; Manual indentation in insert state (I usually use automatic indentation).
  (evil-define-key 'insert 'global (kbd "C-t") nil)
  (evil-define-key 'insert 'global (kbd "C-d") nil)

  ;; Completion (I use another package for that).
  (evil-define-key 'insert 'global (kbd "C-n") nil)
  (evil-define-key 'insert 'global (kbd "C-p") nil)

  ;; Vim bindings in the ":" prompt (inconsistent with bindings in other prompts).
  (dolist (key '("C-a" "C-b" "C-c" "C-d" "C-k" "C-l" "C-r" "C-u" "C-v" "C-w" "C-f"))
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
