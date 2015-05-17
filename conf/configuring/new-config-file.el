;;; A function for creating new configuration files.

(defun new-config-file ()
  "Create a new config file (with a simple template)."
  (interactive)
  (let* ((new-file-name (ido-read-file-name "New file: " (expand-file-name "conf" main-dir)))
         (new-feature-name (file-name-sans-extension (file-relative-name new-file-name main-dir))))
    (when (file-exists-p new-file-name)
      (error (concat new-file-name " already exists")))

    (find-file new-file-name)
    (insert ";;; \n"
            "\n"
            "(provide '" new-feature-name ")\n")
    (goto-char (point-min))
    (call-interactively #'evil-append-line)))

(provide 'conf/configuring/new-config-file)
