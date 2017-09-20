;;; Color theme. -*- lexical-binding: t -*-

;; Create `after-load-theme-hook'.
;; TODO submit this upstream?
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; TODO submit this upstream?
(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough (see <https://emacs.stackexchange.com/a/19242>).
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(when (>= (display-color-cells) 16)
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
    (load-theme 'sanityinc-tomorrow-night t)
    ;; Disable italics.
    (dolist (face '(font-lock-comment-delimiter-face
                    font-lock-comment-face mode-line-emphasis))
      (set-face-attribute face nil :slant 'normal))
    (with-eval-after-load 'rhtml-fonts
      (set-face-attribute 'erb-comment-face nil :slant 'normal))))

;; Don't let Evil set the cursor color.
(with-eval-after-load 'evil
  (setq evil-default-cursor nil))

(provide 'conf/view/color-theme)
