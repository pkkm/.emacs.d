;;; When opening a file in a non-existing directory, offer to create it. -*- lexical-binding: t -*-
;; Source: <http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/>.

(defun my-create-non-existent-dir ()
  (let ((parent-dir (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-dir))
               (y-or-n-p (format "Directory `%s' does not exist! Create?" parent-dir)))
      (make-directory parent-dir t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-dir)

(provide 'conf/opening-saving/create-nonexistent-dirs)
