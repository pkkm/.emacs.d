;;; Key bindings for file-related commands.

(require 'conf/evil)
(require 'conf/minibuffer/ido)

;; Find (open) file in the config directory.
(global-set-key (kbd "C-c c") #'find-file-in-conf)
(defun find-file-in-conf ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "conf" main-dir)))

(require 'conf/configuring/new-config-file)
(global-set-key (kbd "C-c n") #'new-config-file)

;; Revert.
(global-set-key (kbd "C-c v") #'revert-buffer)

;; Rename/delete file (and buffer).
(require 'conf/opening-saving/move-delete)
(global-set-key (kbd "C-c d") #'delete-this-buffer-and-file)
(global-set-key (kbd "C-c m") #'move-this-buffer-and-file)

;; Ag -- like grep, but better for source code.
(require 'conf/opening-saving/ag)
(global-set-key (kbd "C-c a") #'ag)
(global-set-key (kbd "C-c A") #'ag-project)

;; Magit -- Git interface.
(require 'conf/opening-saving/version-control)
(global-set-key (kbd "C-c g") #'magit-status)

(provide 'conf/opening-saving/keys)
