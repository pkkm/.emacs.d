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

;; Rename/delete file (and buffer).
(require 'conf/opening-saving/move-delete)
(global-set-key (kbd "C-c d") #'delete-this-buffer-and-file)
(global-set-key (kbd "C-c m") #'move-this-buffer-and-file)

;; Ag -- like grep, but better for source code.
(with-eval-after-load 'ag
  (global-set-key (kbd "C-c a") #'ag)
  (global-set-key (kbd "C-c A") #'ag-project))

;; Magit -- Git interface.
(with-eval-after-load 'magit
  (global-set-key (kbd "C-c g") #'magit-status))

(provide 'conf/opening-saving/keys)
