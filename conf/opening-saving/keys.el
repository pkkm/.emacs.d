;;; Key bindings for file-related commands.

;; Find (open) file in the config directory.
(with-eval-after-load 'ido
  (defun find-file-in-conf ()
    (interactive)
    (ido-find-file-in-dir (expand-file-name "conf" main-dir)))
  (global-set-key (kbd "C-c c") #'find-file-in-conf))

;; New config file.
(require 'conf/configuring/new-config-file)
(global-set-key (kbd "C-c n") #'new-config-file)

;; Revert.
(global-set-key (kbd "C-c v") #'revert-buffer)

(provide 'conf/opening-saving/keys)
