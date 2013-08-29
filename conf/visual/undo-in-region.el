;;; Undo the last modification in region (selection).
;;; Does not work at end of file!

(require 'conf/evil)

;; In visual state, use "u" for undoing instead of lowercasing ("g u").
;; ("C-r" already redoes.)
(define-key evil-visual-state-map (kbd "u") 'undo-tree-undo)

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

(provide 'conf/visual/undo-in-region)
