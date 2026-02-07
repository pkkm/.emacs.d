;;; Repeat command with RET. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (evil-define-key 'motion 'global (kbd "g SPC") #'repeat))

(provide 'conf/evil-specific/repeat-command)
