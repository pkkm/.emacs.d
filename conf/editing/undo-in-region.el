;;; Undo the last modification in region. -*- lexical-binding: t -*-
;;; Does not work at end of file!

;; In visual state, use "u" for undoing instead of lowercasing ("g u").
;; ("C-r" already redoes.)
(with-eval-after-load 'evil
  (bind-key "u" #'undo-tree-undo evil-visual-state-map))

;; Preserve region when undoing.
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((mark-before-undo (set-marker (make-marker) (mark)))
            (point-before-undo (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char point-before-undo)
        (set-mark mark-before-undo)
        (set-marker point-before-undo nil)
        (set-marker mark-before-undo nil))
    ad-do-it))

(provide 'conf/editing/undo-in-region)
