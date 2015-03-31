;;; File backups.
;; To disable, (setq make-backup-files nil).

(setq backup-by-copying t) ; Slower, but won't make hard links point to the backup.

;; Backup directory.
(let ((backup-dir (locate-user-emacs-file "backups")))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(setq delete-old-versions t) ; Delete old backups silently.

(provide 'conf/opening-saving/backups)
