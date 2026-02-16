;;; Move/delete file. -*- lexical-binding: t -*-

;; Just like the built-in `rename-visited-file', but uses the current name as the initial value in the prompt.
(defun my-rename-this-buffer-and-file ()
  "Moves (renames) the current buffer and the file it is visiting (after saving it)."
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Not visiting a file"))
    (unless (file-exists-p filename)
      (user-error "File doesn't exist: %s" filename))
    (let ((new-name
           (read-file-name "New name: "
                           (file-name-directory filename)
                           nil nil
                           (file-name-nondirectory filename))))
      (when (or (file-exists-p new-name)
                (file-symlink-p new-name))
        (user-error "File already exists: %s" new-name))
      (rename-file filename new-name)
      (set-visited-file-name new-name)))
  (set-buffer-modified-p nil))
(bind-key "C-c m" #'my-rename-this-buffer-and-file)

(defun my-delete-this-buffer-and-file (no-confirmation)
  "Removes the file visited by the current buffer and kills the buffer."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (unless (and filename (file-exists-p filename))
      (user-error "Not visiting a file"))
    (when (or no-confirmation
              (yes-or-no-p (format "Delete %s?" filename)))
      (delete-file filename t) ; t -- move to thrash if `delete-by-moving-to-trash' is non-nil.
      (unless (file-exists-p filename)
        (kill-buffer)))))
(bind-key "C-c d" #'my-delete-this-buffer-and-file)

(setq delete-by-moving-to-trash t)

(provide 'conf/opening-saving/move-delete)
