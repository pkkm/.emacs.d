;;; Color theme. -*- lexical-binding: t -*-

;; TODO submit this upstream?
(require 'conf/utils/functions) ; Used: define-interactive-wrapper.
(define-interactive-wrapper change-theme (&rest args) load-theme
  "Like `load-theme', but disables all themes before loading the new one."
  (mapcar #'disable-theme custom-enabled-themes)
  (diw-apply-original-fun args))

;; Except for Wombat, the themes below are chosen largely based on their support for various modes (Org, Helm, Magit, etc.).

;; Ample -- dark; mainly warm colors.
;; (use-package ample-theme
;;  :ensure t
;;  :init
;;  (load-theme 'ample t))

;; Monokai -- dark; mainly warm colors.
;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))

;; Wombat -- dark; some warm and some cold colors.
;; (use-package wombat ; Bundled with Emacs.
;;   :init
;;   (load-theme 'wombat)
;;   (set-cursor-color "navajo white")
;;   ;; Different colors for Org headline levels with the Wombat theme.
;;   ;; (Wombat has the same colors for Org headlines with levels 1 and 2, and also 6 and 7.)
;;   (with-eval-after-load 'org
;;     (setq org-level-faces
;;           '(org-level-1 org-level-3 org-level-4 org-level-5 org-level-6 org-level-8))
;;     (setq org-n-level-faces 6)))

;; Tomorrow Night -- dark; mainly cold colors.
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (load-theme 'sanityinc-tomorrow-night t))

;; Theme customizations below. We do them that way so that they work even if the theme is changed manually.

;; Make the background of Tomorrow Night a bit darker.
(require 'conf/utils/colors) ; Used: my-color-mix.
(defun my-tomorrow-night-darker-background (&rest _)
  (when (memq 'sanityinc-tomorrow-night custom-enabled-themes)
    (set-face-background 'default (color-darken-name (face-background 'default nil t) 15))
    ;; Work around a bug where leading stars hidden by org-mode are in the old color.
    (with-eval-after-load 'org
      (set-face-background 'org-hide nil))))
(my-tomorrow-night-darker-background)
(add-hook 'enable-theme-functions #'my-tomorrow-night-darker-background)

;; Disable italics in color themes, e.g. the Tomorrow ones.
(defun my-disable-italics-in-color-themes (&rest _)
  (dolist (face '(font-lock-comment-delimiter-face
                  font-lock-comment-face mode-line-emphasis))
    (set-face-attribute face nil :slant 'normal))
  (with-eval-after-load 'rhtml-fonts
    (set-face-attribute 'erb-comment-face nil :slant 'normal)))
(my-disable-italics-in-color-themes)
(add-hook 'enable-theme-functions #'my-disable-italics-in-color-themes)

;; Don't let Evil set the cursor color.
(with-eval-after-load 'evil
  (setq evil-default-cursor nil))

(provide 'conf/view/color-theme)
