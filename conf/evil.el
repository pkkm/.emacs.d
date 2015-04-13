;;; Evil (Extensible VI Layer).

(use-package evil
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :preface

  ;; Make Evil's > and < always indent by `tab-width'.
  ;; Has to happen before installing or loading Evil, because Evil makes `evil-shift-width' buffer local, and therefore impossible to declare as an alias. (`:init' section of `use-package' executes after installation.)
  (defvaralias 'evil-shift-width 'tab-width)

  :init
  (evil-mode 1)

  :config

  (setq evil-cross-lines t) ; Allow "f" and similar motions to cross newlines.
  (setq evil-move-cursor-back nil) ; Allow cursor at end of line, don't move it back when exiting insert state.

  ;; Search.
  (setq evil-regexp-search nil) ; Don't use regexes for / and ?.
  (setq evil-flash-delay 999999) ; Number of seconds to highlight matches for.

  (setq evil-echo-state nil) ; Don't echo state in the echo area (minibuffer).


  ;;; Unsetting keybindings.

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
  (evil-set-initial-state 'occur-mode 'motion))

(provide 'conf/evil)
