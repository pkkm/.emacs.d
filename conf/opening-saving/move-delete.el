;;; Move/delete file.

(defun move-this-buffer-and-file ()
  "Moves (renames) the current buffer and the file it is visiting (after saving it)."
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Not visiting a file"))
    (unless (file-exists-p filename)
      (error "File \"%s\" doesn't exist" filename))
    (let ((new-name
           (read-file-name "New name: "
                           (file-name-directory filename)
                           nil nil
                           (file-name-nondirectory filename))))
      (when (or (file-exists-p new-name)
                (file-symlink-p new-name))
        (error "File \"%s\" already exists" new-name))
      (rename-file filename new-name)
      (rename-buffer new-name t) ; t -- if the name is taken, pick an unique one.
      (set-visited-file-name new-name)))
  (set-buffer-modified-p nil))
(bind-key "C-c m" #'move-this-buffer-and-file)

(defun delete-this-buffer-and-file ()
  "Removes the file visited by the current buffer and kills the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Not visiting a file.")
      (when (yes-or-no-p "Delete this file? ")
        (delete-file filename t) ; t -- move to thrash (when on Windows). Always returns `nil'.
        (unless (file-exists-p filename)
          (kill-buffer))))))
(bind-key "C-c d" #'delete-this-buffer-and-file)

(provide 'conf/opening-saving/move-delete)
