;;; Org-mode.

(use-package org
  :ensure t
  :defer t
  :config


  ;;; Keybindings.

  ;; Move/promote/demote headline.
  (bind-key "M-h" #'org-metaleft org-mode-map) ; Shadows org-mark-element.
  (bind-key "M-t" #'org-metadown org-mode-map)
  (bind-key "M-n" #'org-metaup org-mode-map)
  (bind-key "M-s" #'org-metaright org-mode-map)

  ;; Move/promote/demote subtree.
  (bind-key "M-H" #'org-shiftmetaleft org-mode-map)
  (bind-key "M-T" #'org-shiftmetadown org-mode-map)
  (bind-key "M-N" #'org-shiftmetaup org-mode-map)
  (bind-key "M-S" #'org-shiftmetaright org-mode-map)

  ;; Set state/priority.
  (bind-key "C-M-h" #'org-shiftleft org-mode-map)
  (bind-key "C-M-t" #'org-shiftdown org-mode-map)
  (bind-key "C-M-n" #'org-shiftup org-mode-map)
  (bind-key "C-M-s" #'org-shiftright org-mode-map)

  ;; Mark element (to mark a subtree, use C-c @).
  (bind-key "M-v" #'org-mark-element org-mode-map) ; Normally would be M-h, but shadowed by previous binding.

  ;; Show all TODOs.
  (bind-key "C-c M-t" #'org-show-todo-tree org-mode-map)

  ;; Navigation (with conf/useless-without-evil/convenient-prefix-keys active):
  ;;   SPC u -- up heading.
  ;;   SPC f -- forward heading (same level).
  ;;   SPC b -- backward heading (same level).

  ;; Make RET also indent.
  (bind-key [remap org-return] #'org-return-indent org-mode-map)

  (with-eval-after-load 'evil
    ;; Insert heading.
    (evil-define-key 'normal org-mode-map (kbd "C-c RET") #'evil-org-insert-heading)

    ;; Replace the normal Evil end-of-line with an org-specific one.
    (evil-define-key 'motion org-mode-map [remap evil-end-of-line] #'org-end-of-line)

    ;; Normalize keymaps.
    ;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
    ;; See <https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not>.
    (evil-normalize-keymaps)

    (defun evil-org-insert-heading ()
      "Insert a heading in Org-Mode and switch to Evil's insert state."
      (interactive)
      (org-end-of-line)
      (call-interactively #'org-insert-heading)
      (unless (evil-insert-state-p)
        (evil-append 1))))


  ;;; Other.

  ;; Display.
  (setq org-startup-truncated nil) ; Wrap long lines instead of truncating them (toggle with `toggle-truncate-lines').
  (setq org-src-fontify-natively t) ; Fontify code blocks.
  (setq org-hide-leading-stars t) ; Make leading stars of headlines background color.
  (setq org-fontify-done-headline t) ; Mark the whole headline of a DONE task with a different face.

  ;; Inline LaTeX formula rendering (Org recognizes "\(", "\[", etc.).
  ;; To render: C-c C-x C-l, to undo: C-c C-c. To render on startup: "#+STARTUP: latexpreview".
  (plist-put org-format-latex-options :scale 1.8) ; Larger formulas.

  ;; Ellipsis style for folded sections.
  (require 'conf/utils/colors) ; Used: color-mix.
  (setq org-ellipsis 'org-ellipsis)
  (defun set-org-ellipsis-style ()
    "Calculate the modeline backgrounds for various Evil states."
    (let* ((base-color "cyan")
           (color (color-mix base-color 0.4 (face-attribute 'default :foreground) 0.6))
           (box-color (color-mix base-color 0.15 (face-attribute 'default :background) 0.85)))
      (face-spec-set 'org-ellipsis
                     `((t (:foreground ,color :box (:line-width 1 :color ,box-color :style nil)))))))
  (add-hook 'after-load-theme-hook #'set-org-ellipsis-style)
  (set-org-ellipsis-style)

  ;; Logging.
  (setq org-log-repeat nil) ; Don't log shifting forward the date of a repeating task.

  ;; Completion.
  (setq org-completion-use-ido t))

(provide 'conf/mode-specific/org)
