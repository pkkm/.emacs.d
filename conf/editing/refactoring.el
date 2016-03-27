;;; Refactoring.
;; For a list of supported languages, see <https://github.com/chrisbarrett/emacs-refactor>.

(use-package emr
  :ensure t
  :init
  (bind-key "C-c \\" #'emr-show-refactor-menu prog-mode-map)
  (add-hook 'prog-mode-hook #'emr-initialize))

(provide 'conf/editing/refactoring)
