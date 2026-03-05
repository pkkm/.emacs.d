;;; Key bindings for file-related commands. -*- lexical-binding: t -*-

;; Find (open) file in the config directory.
(with-eval-after-load 'counsel
  (defun my-find-file-in-conf ()
    (interactive)
    (counsel-find-file nil (expand-file-name "conf" main-dir)))
  (bind-key "C-c c" #'my-find-file-in-conf))

;; Revert.
(bind-key "C-c v" #'revert-buffer)

(provide 'conf/opening-saving/bindings)
