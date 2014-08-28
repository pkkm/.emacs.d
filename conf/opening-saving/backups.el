;;; File backups.
;;; To disable, (setq make-backup-files nil).

(setq make-backup-files t
      vc-make-backup-files nil) ; Don't backup files in version control.

(setq backup-by-copying t) ; Slower, but won't make hard links point to the backup.

;; Backup directory.
(let ((backup-dir (locate-user-emacs-file "backups")))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(setq delete-old-versions t ; Delete old backups silently.
      kept-new-versions 4 ; Keep 4 newest versions.
      kept-old-versions 2) ; Keep 2 oldest versions.

(provide 'conf/opening-saving/backups)
