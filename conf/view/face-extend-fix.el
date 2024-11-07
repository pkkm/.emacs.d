;;; Fix the `:extend' face attribute in some packages. -*- lexical-binding: t -*-

(defun my-set-faces-extend (faces)
  "Set `:extend t' on every face in FACES."
  (dolist (face faces)
    (set-face-extend face t)))

(with-eval-after-load 'org
  (when (version< (org-version) "9.3.8") ; org-mode commit 81e2948.
    (my-set-faces-extend
     '(org-block org-block-begin-line org-block-end-line org-level-1))))

;; Still needed in version 2.5 (latest as of 2022-08).
(with-eval-after-load 'markdown-mode
  (my-set-faces-extend '(markdown-code-face)))

;; Needed in Emacs 28. Check again when I upgrade to 29.
(with-eval-after-load 'pulse
  (my-set-faces-extend '(pulse-highlight-start-face)))

(provide 'conf/view/face-extend-fix)
