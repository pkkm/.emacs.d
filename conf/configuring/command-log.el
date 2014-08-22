;;; Command log mode.

(require 'conf/packages)

(use-package command-log-mode ; Installed in `my-vendor-dir'.
  :defer t
  :bind (("C-x l" . command-log-mode)
         ("C-x L" . clm/open-command-log-buffer)))

(provide 'conf/configuring/command-log)
