;;; Auto-saving. -*- lexical-binding: t -*-

;; Savefiles for the lists of auto-save files.
;; Despite `user-emacs-directory' being set in init.el to my directory for savefiles, auto-save file lists would still be stored under ~/.emacs.d/auto-save-list (something in `startup.el' seems to be responsible for this), so we have to set this manually.
(setq auto-save-list-file-prefix
      (locate-user-emacs-file "auto-save-file-list/"))

;; When killing a buffer and there's an auto-save file, offer to delete it.
(setq kill-buffer-delete-auto-save-files t)

(provide 'conf/opening-saving/auto-save)
