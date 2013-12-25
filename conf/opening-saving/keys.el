;;; Key bindings for file-related commands.

(require 'conf/evil)
(require 'conf/minibuffer/ido)

;; Find (open) file.
(require 'conf/view/buffer-map) ; To define keys in `my-buffer-map'.
(define-key my-buffer-map (kbd "e") #'ido-find-file)
(global-set-key (kbd "C-c e") #'ido-find-file)

;; Find (open) file in the config directory.
(global-set-key (kbd "C-c c") #'find-file-in-conf)
(defun find-file-in-conf ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "conf" main-dir)))

(require 'conf/configuring/new-config-file)
(global-set-key (kbd "C-c n") #'new-config-file)

;; Insert the contents of a file into the buffer.
(global-set-key (kbd "C-c i") #'ido-insert-file)

;; Save.
(global-set-key (kbd "C-c s") #'save-buffer)

;; Write (save as).
(global-set-key (kbd "C-c w") #'ido-write-file)

;; Save region.
(global-set-key (kbd "C-c R") #'write-region)

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
