;;; Key bindings for file-related commands. -*- lexical-binding: t -*-

;; Find (open) file in the config directory.
(with-eval-after-load 'ido
  (defun find-file-in-conf ()
    (interactive)
    (ido-find-file-in-dir (expand-file-name "conf" main-dir)))
  (bind-key "C-c c" #'find-file-in-conf))

;; Revert.
(bind-key "C-c v" #'revert-buffer)

(provide 'conf/opening-saving/bindings)
