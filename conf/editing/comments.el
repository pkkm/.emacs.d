;;; Handling comments.

(require 'conf/evil)

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

;; Undefine the "z" and "Z" prefixes; I never use them  anyway (C-l is sufficient for scrolling).
(define-key evil-normal-state-map (kbd "z") nil)
(define-key evil-motion-state-map (kbd "z") nil)
(define-key evil-visual-state-map (kbd "z") nil)
(define-key evil-normal-state-map (kbd "Z") nil)

;; z -- my Evil-compatible version of comment-dwim.
;; If region is active, comment/uncomment it. Otherwise, insert a right-margin comment.
(defun evil-comment-dwim ()
  (interactive)
  "Like `comment-dwim', but switches to Insert state when inserting a comment and not operating on a region."
  (unless (and mark-active transient-mark-mode) ; Same check as in `newcomment.el' (this could be done using `use-region-p' instead).
    (unless (evil-insert-state-p)
      (evil-insert-state)))
  (call-interactively #'comment-dwim))
(define-key evil-normal-state-map (kbd "z") #'evil-comment-dwim)

;; Z -- Evil operator to comment/uncomment a piece of text (works with region too).
(package-ensure-installed 'evil-nerd-commenter)
(setq evilnc-hotkey-comment-operator (kbd "Z")) ; This package has to be supplied with a key to bind to the operator. (This is just stupid, TODO submit a bug request?)
(autoload 'evilnc-comment-operator "evil-nerd-commenter"
  "Evil operator to comment/uncomment a piece of text.")
;; We define the keys even though `evil-nerd-commenter' does it, so that they are defined before loading it.
(define-key evil-normal-state-map (kbd "Z") #'evilnc-comment-operator)
(define-key evil-visual-state-map (kbd "Z") #'evilnc-comment-operator)

(provide 'conf/editing/comments)
