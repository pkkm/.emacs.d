;;; Expand-region: expant/contract selection by semantic units.

(use-package expand-region
  :ensure t
  :defer t
  :init

  (with-eval-after-load 'evil
    (evil-define-command evil-visual-char-or-expand-region ()
      "Switch to character (default) selection in Evil visual state. If we're already in it, expand region."
      :keep-visual t ; Keep point where it is instead of moving it to the beginning/end of visual selection. Otherwise, switching from visual line to visual char would move the point.
      (if (eq evil-visual-selection 'char)
          (call-interactively #'er/expand-region)
        (call-interactively #'evil-visual-char)))

    (bind-key "v" #'evil-visual-char-or-expand-region evil-visual-state-map)
    (bind-key "M-v" #'er/contract-region evil-visual-state-map)))

(provide 'conf/region/expand-region)
