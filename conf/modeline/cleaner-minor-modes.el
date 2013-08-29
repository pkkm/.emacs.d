;;; Change certain minor mode names.

(defvar mode-line-cleaner-minor-mode-alist
  ;; Some greek characters: ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ
  `((writegood-mode " WriteGood")
    (auto-complete-mode " α")
    (yas-minor-mode " γ")
    (volatile-highlights-mode " υ")

    (smartparens-mode "")
    (indent-guide-mode "")
    (undo-tree-mode "")
    (page-break-lines-mode ""))
  "An alist of minor mode name abbreviations.")

(require 'cl-lib)
(require 'cl) ; Used: lexical-let*.
(defun mode-line-clean-minor-mode-alist ()
  "Substitute the abbreviated minor mode names from `mode-line-cleaner-minor-mode-alist' for the full names."
  (cl-loop for mode-and-name in mode-line-cleaner-minor-mode-alist
           do (lexical-let* (mode new-name)
                (cl-destructuring-bind (mode new-name) mode-and-name
                  (if (assq mode minor-mode-alist)
                      ;; If there's already an entry for `mode', modify it.
                      (setcar (cdr (assq mode minor-mode-alist)) new-name)
                    ;; Otherwise, schedule its modification for when the mode is activated.
                    (add-hook (intern (concat (symbol-name mode) "-hook"))
                              (lambda ()
                                (setcar (cdr (assq mode minor-mode-alist)) new-name))))))))

(mode-line-clean-minor-mode-alist)

(provide 'conf/modeline/cleaner-minor-modes)
