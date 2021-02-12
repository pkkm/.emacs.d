;;; Managing my private ELPA repository (used to keep package versions stable and consistent across my computers). -*- lexical-binding: t -*-

;; How to update:
;; 1. rm -r ~/.emacs.d/elpa
;; 2. rm ~/.emacs.d/repo/* # Optional -- only if you want to update packages to their newest versions.
;; 3. emacs # Wait for all packages to be installed.
;; 4. M-x my-elpa-repo-recreate
;; 5. cd ~/.emacs.d/repo && git add . && git commit -m "Update" && git push

(use-package elpa-mirror
  :ensure t
  :init

  (defun my-elpa-repo-recreate ()
    "Recreate the ELPA repository in `my-elpa-repo-dir' from installed packages."
    (interactive)
    (let ((temp-dir (expand-file-name "repo-temp" main-dir)))
      (elpamr-create-mirror-for-installed temp-dir t)
      (when (file-exists-p my-elpa-repo-dir)
        (dolist (file (directory-files my-elpa-repo-dir t))
          (unless (member (file-name-nondirectory file) '("." ".." ".git"))
            (f-delete file t))))
      (copy-directory temp-dir my-elpa-repo-dir nil t t)
      (delete-directory temp-dir t)
      (message "Recreated repository in %s" my-elpa-repo-dir))))

(provide 'conf/other/my-elpa-repo)
