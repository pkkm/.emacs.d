;;; Support for editing CMakeLists.txt files. -*- lexical-binding: t -*-

(use-package cmake-mode
  :ensure t)

;; Advanced syntax highlighting (far better than cmake-mode's).
(use-package cmake-font-lock
  :ensure t
  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(provide 'conf/mode-specific/cmake)
