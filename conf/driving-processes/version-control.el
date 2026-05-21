;;; Version control interface. -*- lexical-binding: t -*-

;; Magit.
(use-package magit
  :ensure t
  :preface
  (setq magit-define-global-key-bindings nil) ; Disable default global bindings.
  :bind (("C-c g" . magit-status)
         ("C-c f" . magit-file-dispatch))
  :init

  ;; Keybindings for my `git-quick' script.
  (defun my-magit-quick-sync ()
    (interactive)
    (magit-git-command "git quick --sync"))
  (bind-key "C-c s" #'my-magit-quick-sync)
  (defun my-magit-quick-commit-and-sync ()
    (interactive)
    (magit-git-command "git quick --add --commit --sync"))
  (bind-key "C-c S" #'my-magit-quick-commit-and-sync)

  :config

  ;; Make `magit-status' take up the whole screen. When exiting it, restore the closed windows.
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)

  ;; Start writing commit messages in insert mode.
  (with-eval-after-load 'evil
    (add-hook 'git-commit-setup-hook 'evil-insert-state)))

(use-package diff-hl
  :ensure t
  :init

  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)

  :config

  (setq diff-hl-update-async (if (> emacs-major-version 30) t 'thread))
  (setq diff-hl-show-staged-changes nil)

  ;; Refresh on Magit refresh and insert state exit.
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (with-eval-after-load 'evil
    (defun my-diff-hl-enable-updating-on-insert-state-exit ()
      (if diff-hl-flydiff-mode
          (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
        (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))
    (add-hook 'diff-hl-flydiff-mode-hook #'my-diff-hl-enable-updating-on-insert-state-exit)))

(provide 'conf/driving-processes/version-control)
