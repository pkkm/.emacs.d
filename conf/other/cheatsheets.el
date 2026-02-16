;;; Opening external cheatsheets. -*- lexical-binding: t -*-

(defun my-cheatsheet (url &optional file-name)
  "Download URL to FILE-NAME if it doesn't already exist, then open it with Zathura."
  (let* ((cheatsheet-dir (locate-user-emacs-file "cheatsheets"))
         (name (or file-name (url-file-nondirectory url)))
         (file-path (expand-file-name name cheatsheet-dir)))
    (make-directory cheatsheet-dir t)
    (unless (file-exists-p file-path)
      (url-copy-file url file-path))
    (start-process "zathura-cheatsheet" nil "zathura" "--" file-path)))

(provide 'conf/other/cheatsheets)
