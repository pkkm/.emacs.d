;;; Handling comments.

;; Never indent right-margin comments.
(defvar my-never-indent-right-margin-comments t
  "Force disable indenting right-margin comments in all modes.")
(when my-never-indent-right-margin-comments
  (setq-default comment-column 0))
(defun my-dont-indent-right-margin-comments ()
  "If `my-never-indent-right-margin-comments' is non-nil, turn off indenting right-margin comments (set `comment-column' to 0 in the current buffer)."
  (when my-never-indent-right-margin-comments
    (setq comment-column 0)))
(add-hook 'after-change-major-mode-hook #'my-dont-indent-right-margin-comments)

(package-ensure-installed 'evil-nerd-commenter) ; So that it's installed even if Evil isn't enabled at the moment.
(with-eval-after-load 'evil
  ;; Undefine the "z" and "Z" prefixes; I never use them anyway (C-l is sufficient for scrolling).
  (dolist (keymap (list evil-normal-state-map evil-motion-state-map evil-visual-state-map))
    (bind-key "z" nil keymap))
  (bind-key "Z" nil evil-normal-state-map)

  ;; z -- my Evil-compatible version of comment-dwim.
  ;; If region is active, comment/uncomment it. Otherwise, insert a right-margin comment.
  (defun evil-comment-dwim ()
    (interactive)
    "Like `comment-dwim', but switches to Insert state when inserting a comment and not operating on a region."
    (unless (and mark-active transient-mark-mode) ; Same check as in `newcomment.el' (this could be done using `use-region-p' instead).
      (unless (evil-insert-state-p)
        (evil-insert-state)))
    (call-interactively #'comment-dwim))
  (bind-key "z" #'evil-comment-dwim evil-normal-state-map)

  ;; Z -- Evil operator to comment/uncomment a piece of text (works with region too).
  (use-package evil-nerd-commenter
    :commands evilnc-comment-operator
    :init

    ;; This package has to be supplied with a key to bind to the operator. (TODO submit a bug report?)
    (setq evilnc-hotkey-comment-operator (kbd "Z"))

    ;; We define the keys even though `evil-nerd-commenter' does it, so that they are defined before loading it.
    (dolist (keymap (list evil-normal-state-map evil-visual-state-map))
      (bind-key "Z" #'evilnc-comment-operator keymap))))

(provide 'conf/editing/comments)
