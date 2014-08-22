;;; Settings for games.

(use-package gamegrid ; Bundled with Emacs.
  :defer t
  :config

  ;; Save game scores in `my-savefile-dir'/games.
  (setq gamegrid-user-score-file-directory
        (expand-file-name "games" my-savefile-dir)))

(provide 'conf/other/games)
