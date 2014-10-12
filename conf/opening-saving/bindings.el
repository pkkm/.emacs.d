;;; Key bindings for file-related commands.

;; Find (open) file in the config directory.
(with-eval-after-load 'ido
  (defun find-file-in-conf ()
    (interactive)
    (ido-find-file-in-dir (expand-file-name "conf" main-dir)))
  (bind-key "C-c c" #'find-file-in-conf))

;; New config file.
(require 'conf/configuring/new-config-file)
(bind-key "C-c n" #'new-config-file)

;; Revert.
(bind-key "C-c v" #'revert-buffer)

(provide 'conf/opening-saving/bindings)
