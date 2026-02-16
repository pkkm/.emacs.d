;;; Support for editing CMakeLists.txt files. -*- lexical-binding: t -*-
;; TODO: Is this still necessary now that we have `cmake-ts-mode' in Emacs 29+?

(use-package cmake-mode
  :ensure t)

;; Advanced syntax highlighting (far better than cmake-mode's).
(use-package cmake-font-lock
  :ensure t
  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(provide 'conf/mode-specific/cmake)
