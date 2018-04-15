;;; Remember recently opened files and allow quick access to them. -*- lexical-binding: t -*-

(use-package recentf ; Bundled with Emacs.
  :init

  ;; Has to be set before loading `recentf'.
  (setq recentf-auto-cleanup 120) ; Clean nonexistent files after 2 min of idle. Trigger manually with `recentf-cleanup'.
  (setq recentf-max-saved-items 1000)

  (recentf-mode 1)

  (with-eval-after-load 'ido
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to open a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Abort")))
    (bind-key "C-c r" #'ido-recentf-open)))

;; Sync the recent file list between Emacs instances (by default, the last-closed instance overwrites other ones).
(use-package sync-recentf
  :ensure t
  :init
  (with-eval-after-load 'recentf
    (require 'sync-recentf)))

(provide 'conf/opening-saving/recent-files)
