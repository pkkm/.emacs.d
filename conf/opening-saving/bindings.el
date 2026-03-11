;;; Key bindings for file-related commands. -*- lexical-binding: t -*-

;; Find (open) file in the config directory.
(defun my-find-file-in-conf ()
  (interactive)
  (let ((default-directory (expand-file-name "conf/" main-dir)))
    (call-interactively #'find-file)))
(bind-key "C-c c" #'my-find-file-in-conf)

;; Revert.
(bind-key "C-c v" #'revert-buffer)

(provide 'conf/opening-saving/bindings)
