;;; Require other parts of this configuration: every .el file in conf/ except this one.
;; Example: for file "~/.emacs.d/conf/view/scrolling.el", 'conf/view/scrolling will be required.
;; The order of `require's must not be significant (they are treated as a DAG by my `flatten-conf' utility).

(use-package f :ensure t :commands (f-files f-no-ext f-relative f-ext?))
 (mapc (lambda (file)
         (let ((feature-name (f-no-ext (f-relative file main-dir))))
           (unless (equal feature-name "conf/main") ; Prevents circular `require'.
             (require (intern feature-name)))))
       (f-files (expand-file-name "conf" main-dir)
                (lambda (file) (f-ext? file "el"))
                t))

;; Little used parts of config (candidates for deletion):
;;   * conf/driving-processes/isend

(provide 'conf/main)
