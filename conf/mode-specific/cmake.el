;;; Support for editing CMakeLists.txt files.

(use-package cmake-mode
  :ensure t
  :defer t)

;; Advanced syntax highlighting (far better than cmake-mode's).
(use-package cmake-font-lock
  :ensure t
  :defer t
  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(provide 'conf/mode-specific/cmake)
