;;; Org-mode.

(require 'conf/packages)

(package-ensure-installed 'org)

(require 'conf/utils/hooks)
(add-hook 'org-mode-hook #'my-org-mode-customization)
(add-one-shot-hook 'org-mode-hook #'my-org-mode-bindings)

(defun my-org-mode-customization ()
  "Customizations that should be applied every time Org-mode is enabled."
  ;; Don't use a changed definition of a paragraph.
  (kill-local-variable 'paragraph-start)
  (kill-local-variable 'paragraph-separate))

(defun my-org-mode-bindings ()
  "Add some bindings to Org-mode's keymap.
This function needs to be run only once in an Emacs session."
  ;; Move/promote/demote headline.
  (define-key org-mode-map (kbd "M-h") #'org-metaleft) ; Shadows org-mark-element.
  (define-key org-mode-map (kbd "M-t") #'org-metadown)
  (define-key org-mode-map (kbd "M-n") #'org-metaup)
  (define-key org-mode-map (kbd "M-s") #'org-metaright)

  ;; Move/promote/demote subtree.
  (define-key org-mode-map (kbd "M-H") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-T") #'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-N") #'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-S") #'org-shiftmetaright)

  ;; Set state/priority.
  (define-key org-mode-map (kbd "C-M-h") #'org-shiftleft)
  (define-key org-mode-map (kbd "C-M-t") #'org-shiftdown)
  (define-key org-mode-map (kbd "C-M-n") #'org-shiftup)
  (define-key org-mode-map (kbd "C-M-s") #'org-shiftright)

  ;; Insert heading.
  (evil-define-key 'normal org-mode-map (kbd "C-c RET") #'evil-org-insert-heading)
  (define-key org-mode-map (kbd "M-o") #'evil-org-insert-heading)
  (define-key org-mode-map (kbd "C-M-o") #'evil-org-insert-todo-heading)

  ;; Show all TODOs.
  (define-key org-mode-map (kbd "C-c M-t") #'org-show-todo-tree)

  ;; Navigation (with conf/other/convenient-prefix-keys active):
  ;;   SPC u -- up heading.
  ;;   SPC f -- forward heading (same level).
  ;;   SPC b -- backward heading (same level).

  ;; Replace the normal end-of-line with an org-specific one.
  (evil-define-key 'motion org-mode-map [remap evil-end-of-line] #'org-end-of-line)

  ;; Make RET also indent in insert mode.
  (define-key org-mode-map [remap org-return] #'org-return-indent)

  ;; Normalize keymaps.
  ;; This is necessary for bindings defined using `evil-define-key' to be active before the first Evil state change.
  ;; See <https://bitbucket.org/lyro/evil/issue/301/evil-define-key-for-minor-mode-does-not>.
  (evil-normalize-keymaps))

(defun evil-org-insert-heading ()
  "Insert a heading in Org-Mode and switch to Evil's insert state."
  (interactive)
  (org-end-of-line)
  (call-interactively #'org-insert-heading)
  (unless (evil-insert-state-p)
    (evil-append 1)))

(defun evil-org-insert-todo-heading ()
  "Insert a TODO heading in Org-Mode and switch to Evil's insert state."
  (interactive)
  (org-end-of-line)
  (call-interactively #'org-insert-todo-heading)
  (unless (evil-insert-state-p)
    (evil-append 1)))

;; Display.
(setq org-startup-truncated nil) ; Wrap long lines instead of truncating them (toggle with `toggle-truncate-lines').
(setq org-src-fontify-natively t) ; Fontify code blocks.
(setq org-hide-leading-stars t) ; Make leading stars of headlines background color.
(setq org-fontify-done-headline t) ; Mark the whole headline of a DONE task with a different face.

;; Agenda.
(setq org-agenda-files
      (list (when (file-exists-p "~/Org") "~/Org")))
(global-set-key (kbd "C-x C-a") #'org-agenda)

;; Logging.
(setq org-log-repeat nil) ; Don't log shifting forward the date of a repeating task.

(provide 'conf/mode-specific/org)
