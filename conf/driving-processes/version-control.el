;;; Version control interfaces.

;; Magit (git interface).
;; NOTE: before upgrading to Magit 2.1.0, be sure to uninstall `magit', `git-commit-mode' and `git-rebase-mode'.
;; NOTE: Magit 2.1.0 requires Emacs 24.4+.
(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0") ; Don't display instructions which I've already seen.
  :config

  ;; For old (pre-2.1.0) Magit versions: don't show magit-auto-revert-mode in the modeline.
  (when (fboundp 'magit-auto-revert-mode)
    (diminish 'magit-auto-revert-mode))

  ;; Make `magit-status' take up the whole screen. When exiting it, restore the closed windows.
  (defadvice magit-status (around my-magit-fullscreen activate)
    "Save window configuration and make `magit-status' take up the whole screen."
    (window-configuration-to-register :my-magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun my-magit-fullscreen-quit ()
    "Do what pressing `q' would normally do in `magit-status'.
Then, restore the saved window configuration from before launching `magit-status'."
    (interactive)
    (when my-magit-fulscreen-previous-q-command
      (call-interactively my-magit-fulscreen-previous-q-command))
    (jump-to-register :my-magit-fullscreen))
  (defvar my-magit-fulscreen-previous-q-command
    (lookup-key magit-status-mode-map (kbd "q") t)
    "The command that was bound to `q' in `magit-status' before `q' was rebound.")
  (bind-key "q" #'my-magit-fullscreen-quit magit-status-mode-map)

  ;; Start writing commit message in insert mode.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'git-commit-mode 'insert) ; Pre-2.1.0.
    (add-hook 'with-editor-mode-hook 'evil-insert-state)) ; 2.1.0+.

  ;; When Magit is started, ensure credential cache daemon is running.
  ;; (Necessary for password caching to work as of 2015-04. File Magit feature request?)
  ;; TODO does this apply to 2.1.0+?
  (add-hook 'magit-mode-hook 'my-magit-run-credential-cache-daemon)
  (defun my-magit-run-credential-cache-daemon ()
    (let ((socket-path (expand-file-name "~/.git-credential-cache/socket")))
      (unless (file-exists-p socket-path)
        (let ((process (start-process "credential-cache-daemon" nil "git"
                                      "credential-cache--daemon" socket-path)))
          (set-process-query-on-exit-flag process nil)))))) ; Kill without asking when exiting Emacs.

;; Disable VC-mode (default Emacs interface for VCSes).
(setq vc-handled-backends '())

(provide 'conf/driving-processes/version-control)
