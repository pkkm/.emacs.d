;;; Command log mode.

(global-set-key (kbd "C-x l") #'command-log-mode)
(global-set-key (kbd "C-x L") #'clm/open-command-log-buffer)

(autoload
  'command-log-mode
  "command-log-mode" ; In `my-vendor-dir'.
  "Toggle keyboard command logging."
  t)

(autoload
  'clm/open-command-log-buffer
  "command-log-mode" ; In `my-vendor-dir'.
  "Opens (and creates, if non-existant) a buffer used for logging keyboard commands."
  t)

(provide 'conf/configuring/command-log)
