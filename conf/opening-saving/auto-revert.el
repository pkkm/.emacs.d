;;; Automatically revert buffer when file changes on disk. -*- lexical-binding: t -*-

(global-auto-revert-mode 1)

;; Save power by using inotify or an equivalent instead of polling every `auto-revert-interval' seconds.
(setq auto-revert-avoid-polling t)

(provide 'conf/opening-saving/auto-revert)
