;;; Automate the creation of new config files.

(defun new-config-file ()
  "Create a new config file:
  1. Prompt for the name of the file (in \"conf/\")
  2. Add the appropriate `require' to \"conf/-.el\"
  3. Fill the new file with a simple template."
  (interactive)
  (let* ((new-file-name (ido-read-file-name "New file: " (expand-file-name "conf" main-dir)))
         (new-feature-name (file-name-sans-extension (file-relative-name new-file-name main-dir))))
    (when (file-exists-p new-file-name)
      (error (concat new-file-name " already exists")))

    ;; Add the appropriate `require' to "conf/-.el".
    (find-file (expand-file-name "-.el" (expand-file-name "conf" main-dir)))
    (search-longest-prefix new-feature-name)
    (end-of-line)
    (newline-and-indent)
    (insert "(require '" new-feature-name ")")
    (save-buffer)
    (kill-this-buffer)

    ;; Create the new file.
    (find-file new-file-name)
    (insert ";;; \n"
            "\n"
            "(provide '" new-feature-name ")\n")
    (beginning-of-buffer)
    (call-interactively #'evil-append-line)))

(defun search-longest-prefix (string)
  (let ((match-pos 0) ; The initial value doesn't matter, as long as it's not nil (which signifies no match).
        (prev-match-pos))
    (loop for num-chars from 1 to (length string)
          until (null match-pos)
          do
          (beginning-of-buffer)
          (setq prev-match-pos match-pos)
          (setq match-pos (search-forward (substring string 0 num-chars) nil t)))
    (goto-char prev-match-pos)))

(provide 'conf/configuring/new-config-file)
