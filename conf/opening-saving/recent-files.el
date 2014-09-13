;;; Remember recently opened files and allow quick access to them.

(use-package recentf ; Bundled with Emacs.
  :commands recentf-mode

  :pre-load ; These have to be set before enabling recentf-mode.

  ;; Clean up non-existent files from the list after 2 minutes of idle. Use `recentf-cleanup' to manually trigger this.
  (setq recentf-auto-cleanup 120)

  (setq recentf-max-saved-items 200)

  :init

  (recentf-mode 1)

  (with-eval-after-load 'ido
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to open a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Abort")))
    (bind-key "C-c r" #'ido-recentf-open)))

(provide 'conf/opening-saving/recent-files)
