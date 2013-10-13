;;; Org-mode.

(require 'conf/packages)

(package-ensure-installed 'org)

(require 'conf/utils/hooks)
(add-one-shot-hook 'org-mode-hook #'configure-org-mode-bindings)

(defun configure-org-mode-bindings ()
  "Add some useful bindings to Org-Mode's map."
  (define-key org-mode-map (kbd "M-h") #'org-metaleft) ; Shadows org-mark-element.
  (define-key org-mode-map (kbd "M-t") #'org-metadown)
  (define-key org-mode-map (kbd "M-n") #'org-metaup)
  (define-key org-mode-map (kbd "M-s") #'org-metaright)

  (define-key org-mode-map (kbd "M-H") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "M-T") #'org-shiftmetadown)
  (define-key org-mode-map (kbd "M-N") #'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-S") #'org-shiftmetaright)

  (define-key org-mode-map (kbd "C-M-h") #'org-shiftleft)
  (define-key org-mode-map (kbd "C-M-t") #'org-shiftdown)
  (define-key org-mode-map (kbd "C-M-n") #'org-shiftup)
  (define-key org-mode-map (kbd "C-M-s") #'org-shiftright)

  (define-key org-mode-map (kbd "M-RET") #'evil-org-insert-heading)
  (define-key org-mode-map (kbd "M-o") #'evil-org-insert-heading)
  (define-key org-mode-map (kbd "C-M-o") #'evil-org-insert-todo-heading)

  ;; Navigation (with conf/other/convenient-prefix-keys active):
  ;;   SPC u -- up heading.
  ;;   SPC f -- forward heading (same level).
  ;;   SPC b -- backward heading (same level).

  (evil-define-key 'motion org-mode-map [remap evil-end-of-line] #'org-end-of-line)

  (evil-normalize-keymaps)) ; Necessary for the `evil-define-key' bindings to take effect immediately.

(defun evil-org-insert-heading ()
  "Insert a heading in Org-Mode and switch to Evil's insert state."
  (interactive)
  (org-end-of-line)
  (call-interactively #'org-insert-heading)
  (evil-append 1))

(defun evil-org-insert-todo-heading ()
  "Insert a TODO heading in Org-Mode and switch to Evil's insert state."
  (interactive)
  (org-end-of-line)
  (call-interactively #'org-insert-todo-heading)
  (evil-append 1))

(package-ensure-installed 'org-trello)
;;(defadvice orgtrello-proxy/start (around disable-org-trello-proxy activate)
;;  nil)

(provide 'conf/lang/org)
