;;; Insert a character verbatim. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (bind-key "C-v" #'quoted-insert evil-insert-state-map)
  (bind-key "C-v" #'quoted-insert evil-replace-state-map))

(provide 'conf/evil-specific/insert-verbatim)
