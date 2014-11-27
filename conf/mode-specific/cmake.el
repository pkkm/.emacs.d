;;; Support for editing CMakeLists.txt files.

(use-package cmake-mode
  :ensure cmake-mode
  :defer t)

;; Advanced syntax highlighting (far better than cmake-mode's).
(use-package cmake-font-lock
  :ensure cmake-font-lock
  :defer t
  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(provide 'conf/mode-specific/cmake)
