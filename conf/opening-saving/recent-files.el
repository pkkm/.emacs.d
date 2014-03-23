;;; Remember recently opened files and allow quick access to them.

;; Save in `my-savefile-dir'/recentf.
(setq recentf-save-file
      (expand-file-name "recentf" my-savefile-dir))

(setq recentf-max-saved-items 200)

(setq recentf-auto-cleanup 45) ; Clean up the list after 45 s of idle. See also `recentf-cleanup' for manually triggering it.

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Abort")))
(global-set-key (kbd "C-c r") #'ido-recentf-open)

(recentf-mode 1) ; This can't come before setting `recentf-auto-cleanup'.

(provide 'conf/opening-saving/recent-files)
