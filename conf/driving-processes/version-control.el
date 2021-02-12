;;; Version control interface. -*- lexical-binding: t -*-

;; Magit.
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
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

  ;; Start writing commit message in insert mode.
  (with-eval-after-load 'evil
    (add-hook 'with-editor-mode-hook 'evil-insert-state)))

(provide 'conf/driving-processes/version-control)
