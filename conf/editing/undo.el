;;; Undo customizations. -*- lexical-binding: t -*-

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-enable-undo-in-region nil) ; Should make "primitive-undo: Unrecognized entry in undo list undo-tree-canary" errors less frequent.
  (setq undo-tree-auto-save-history nil)) ; Don't litter directories with `*.~undo-tree~' files.

;; Increase memory limits for undo history (see <https://old.reddit.com/r/emacs/comments/bx82j3>).
(dolist (var '(undo-limit undo-strong-limit undo-outer-limit))
  (set var (* 5 (symbol-value var))))

;; In visual state, use "u" for undoing instead of lowercasing ("g u").
(with-eval-after-load 'evil
  (evil-define-key 'visual 'global (kbd "u") #'undo-tree-undo))

;; Preserve region when undoing.
(defun my-keep-region-when-undoing (orig-fun &rest args)
  (if (use-region-p)
      (let ((mark-before-undo (copy-marker (mark) t))
            (point-before-undo (copy-marker (point) t)))
        (prog1
            (apply orig-fun args)
          (goto-char point-before-undo)
          (set-mark mark-before-undo)
          (set-marker point-before-undo nil)
          (set-marker mark-before-undo nil)))
    (apply orig-fun args)))
(advice-add #'undo-tree-undo :around #'my-keep-region-when-undoing)

(provide 'conf/editing/undo)
