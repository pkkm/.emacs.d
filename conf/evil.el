;;; Evil (Extensible VI Layer).

;; Make Evil's > and < always indent by `tab-width'.
;; This has to happen before loading Evil, because Evil makes `evil-shift-width' buffer local, and therefore impossible to declare as an alias. (I haven't found a way to reverse making variable buffer-local.)
;; This code is before the `use-package' declaration instead of being in the `:pre-load' section because if Evil isn't present, the `:pre-load' section will be executed after it's installed, which is too late.
(defvaralias 'evil-shift-width 'tab-width)

(use-package evil
  :ensure evil
  :defer t
  :diminish undo-tree-mode
  :init
  (evil-mode 1)
  :config

  (setq evil-cross-lines t) ; Allow motions to cross newlines.

  ;; Search.
  (setq evil-regexp-search nil) ; Don't use regexes for / and ?.
  (setq evil-flash-delay 999999) ; Number of seconds to highlight matches for.

  (setq evil-echo-state nil) ; Don't echo state in the echo area (minibuffer).


  ;;; Keybindings.

  ;; In insert state, auto-indent the new line on a press of "RET" (or similar).
  (dolist (fun-to-remap
           '(newline newline-and-indent evil-ret))
    (bind-key (vector 'remap fun-to-remap) #'evil-ret-and-indent evil-insert-state-map))

  ;; Delete word in insert state with C-w.
  (setq evil-want-C-w-delete t)

  ;; Unset other useless keys.
  (bind-key "C-e" nil evil-insert-state-map)
  (bind-key "RET" nil evil-motion-state-map)

  ;; Unset bindings for manual indentation in insert state (I almost always use automatic indentation).
  (bind-key "C-t" nil evil-insert-state-map)
  (bind-key "C-d" nil evil-insert-state-map)

  ;; Don't allow any keymap to shadow Evil bindings.
  (setq evil-overriding-maps '()) ; `evil-overriding-maps' get elevated to the "overriding" status -- above global state keymap, but below the local one.
  (setq evil-pending-overriding-maps '()) ; No idea what the "pending" version is for, but setting only the above doesn't seem to work, for example in compilation-mode.
  ;;(setq evil-intercept-maps '()) ; `evil-intercept-maps' get elevated to the "intercept" status -- above all others. This is useful when debugging.


  ;; Don't allow modes to change the definition of paragraph used by { and }.
  (defun kill-local-paragraph-definitions ()
    "Kill the local values of `paragraph-start' and `paragraph-separate'."
    (kill-local-variable 'paragraph-start)
    (kill-local-variable 'paragraph-separate))
  (add-hook 'after-change-major-mode-hook #'kill-local-paragraph-definitions)

  ;; Initial states in various modes.
  ;; TODO move to files with the configuration for the modes.
  (evil-set-initial-state 'debugger-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'occur-mode 'motion)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'git-commit-mode 'insert))

(provide 'conf/evil)
