;;; Auto-saving (to prevent loss of data).

;; Save the list of auto-save files in "`my-savefile-dir'/auto-save-file-list/saves-<computer ID>".
(package-ensure-installed 'dash) (require 'dash) ; Used: ->>.
(setq auto-save-list-file-prefix
      (->> my-savefile-dir
        (expand-file-name "auto-save-file-list")
        (expand-file-name "saves-")))

(provide 'conf/opening-saving/auto-save)
