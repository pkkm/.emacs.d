;;; Version control interfaces.


;;; Magit (git interface).

(use-package magit
  :ensure magit
  :diminish magit-auto-revert-mode
  :commands magit-status
  :bind ("C-c g" . magit-status)
  :config

  ;; Make `magit-status' take up the whole screen. When exiting it, restore the closed windows.
  (defadvice magit-status (around my-magit-fullscreen activate)
    "Save window configuration, make `magit-status' take up the whole screen and enter recursive edit. When the recursive edit is exited or aborted, the window configuration will be restored."
    (save-window-excursion
      ad-do-it
      (delete-other-windows)
      (let ((my-magit-fullscreen-in-recursive-edit t))
        (recursive-edit))))
  (defun my-magit-fullscreen-quit ()
    "Do what pressing `q' would normally do in `magit-status'.
Then, exit recursive edit, which will restore the window configuration from before launching `magit-status'."
    (interactive)
    (when my-magit-fulscreen-previous-q-command
      (call-interactively my-magit-fulscreen-previous-q-command))
    (when my-magit-fullscreen-in-recursive-edit
      (exit-recursive-edit)))
  (defvar my-magit-fulscreen-previous-q-command
    (lookup-key magit-status-mode-map (kbd "q") t)
    "The command that was bound to `q' in `magit-status' before `q' was rebound.")
  (bind-key "q" #'my-magit-fullscreen-quit magit-status-mode-map))


;;; Disable VC-mode (the default Emacs interface for VCSes).
(setq vc-handled-backends '())


(provide 'conf/opening-saving/version-control)
