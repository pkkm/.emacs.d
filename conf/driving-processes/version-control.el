;;; Version control interface. -*- lexical-binding: t -*-

;; Solve error: "Magit requires `seq' >= 2.24, but due to bad defaults, Emacs’ package manager refuses to upgrade this and other built-in packages to higher releases from GNU Elpa".
(let ((package-install-upgrade-built-in t))
  (package-ensure-version 'seq "2.24" t))

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
    (add-hook 'with-editor-mode-hook 'evil-insert-state)))

(provide 'conf/driving-processes/version-control)
