;;; File backups. -*- lexical-binding: t -*-
;; To disable, (setq make-backup-files nil).

;; TODO: Emacs feature request to add a variable specifying the maximum file size that should be backed up.

(setq backup-by-copying t) ; Slower, but won't make hard links point to the backup.

;; Backup directory.
(let ((backup-dir (locate-user-emacs-file "backups")))
  (setq backup-directory-alist `(("." . ,backup-dir))))

;; Keep more versions (default: 2).
(setq kept-new-versions 4)

;; Delete old backups silently.
(setq delete-old-versions t)

(provide 'conf/opening-saving/backups)
