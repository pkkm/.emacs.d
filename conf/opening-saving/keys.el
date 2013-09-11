;;; Key bindings for file-related commands.

(require 'conf/evil)
(require 'conf/minibuffer/ido)

(defvar my-file-map (make-sparse-keymap)
  "Keymap for file-related commands.")
(define-prefix-command 'my-file-map)
(global-set-key (kbd "C-c") #'my-file-map)

;; Find (open) file.
(require 'conf/view/buffer-map) ; To define keys in `my-buffer-map'.
(define-key my-file-map (kbd "e") #'ido-find-file)
(define-key my-buffer-map (kbd "e") #'ido-find-file)

;; Find (open) file in the config directory.
(define-key my-file-map (kbd "c") #'find-file-in-conf)
(defun find-file-in-conf ()
  (interactive)
  (ido-find-file-in-dir (expand-file-name "conf" main-dir)))

(require 'conf/configuring/new-config-file)
(define-key my-file-map (kbd "n") #'new-config-file)

;; Insert the contents of a file into the buffer.
(define-key my-file-map (kbd "i") #'ido-insert-file)

;; Save.
(define-key my-file-map (kbd "s") #'save-buffer)

;; Write (save as).
(define-key my-file-map (kbd "w") #'ido-write-file)
(define-key my-file-map (kbd "M-s") #'ido-write-file)

;; Save region.
(define-key my-file-map (kbd "R") #'write-region)

;; Revert.
(define-key my-file-map (kbd "v") #'revert-buffer)

;; Rename/delete file (and buffer).
(require 'conf/opening-saving/rename-delete)
(define-key my-file-map (kbd "d") #'delete-this-buffer-and-file)
(define-key my-file-map (kbd "r") #'rename-this-buffer-and-file)

;; Diff
;; TODO
(require 'conf/opening-saving/diff)
(defvar my-file-diff-map (make-sparse-keymap)
  "Keymap for diff commands.")
(define-prefix-command 'my-diff-map)

;; Ag -- like grep, but far better.
(require 'conf/opening-saving/ag)
(define-key my-file-map (kbd "a") #'ag)
(define-key my-file-map (kbd "A") #'ag-project)

(provide 'conf/opening-saving/keys)
