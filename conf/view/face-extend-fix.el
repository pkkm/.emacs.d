;;; Fix the `:extend' face attribute in some packages. -*- lexical-binding: t -*-

(defun my-set-faces-extend (faces)
  "Set `:extend t' on every face in FACES."
  (dolist (face faces)
    (set-face-extend face t)))

(with-eval-after-load 'org
  (when (version< (org-version) "9.3.8") ; org-mode commit 81e2948.
    (my-set-faces-extend
     '(org-block org-block-begin-line org-block-end-line org-level-1))))

(with-eval-after-load 'magit
  (when (version< (magit-version) "3.0.0")
    (my-set-faces-extend
     '(magit-diff-hunk-heading
       magit-diff-hunk-heading-highlight
       magit-diff-hunk-heading-selection
       magit-diff-hunk-region
       magit-diff-lines-heading
       magit-diff-lines-boundary
       magit-diff-conflict-heading
       magit-diff-added
       magit-diff-removed
       magit-diff-our
       magit-diff-base
       magit-diff-their
       magit-diff-context
       magit-diff-added-highlight
       magit-diff-removed-highlight
       magit-diff-our-highlight
       magit-diff-base-highlight
       magit-diff-their-highlight
       magit-diff-context-highlight
       magit-diff-whitespace-warning
       magit-diffstat-added
       magit-diffstat-removed
       magit-section-heading
       magit-section-heading-selection
       magit-section-highlight
       magit-section-secondary-heading
       magit-diff-file-heading
       magit-diff-file-heading-highlight
       magit-diff-file-heading-selection))))

;; Still needed in version 2.5 (latest as of 2022-08).
(with-eval-after-load 'markdown-mode
  (my-set-faces-extend '(markdown-code-face)))

;; Needed in Emacs 28. Check again when I upgrade to 29.
(with-eval-after-load 'pulse
  (my-set-faces-extend '(pulse-highlight-start-face)))

(provide 'conf/view/face-extend-fix)
