;;; Package system.

;; Initialize packages now, instead of after init.
(package-initialize) ; This normally happens after loading the init file.
(setq package-enable-at-startup nil) ; Don't load the packages the second time after the init file.

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; To update installed packages, use M-x package-list-packages RET U x.
;; Or delete the elpa/ directory and launch Emacs for it to be recreated.

(defun package-ensure-installed (package)
  "Ensure the ELPA package PACKAGE is installed."
  (unless (package-installed-p package)
    (unless package-list-refreshed-p
      (package-refresh-contents)
      (setq package-list-refreshed-p t))
    (package-install package)))
(defvar package-list-refreshed-p nil
  "Was the package list refreshed in this Emacs session?")

(provide 'conf/packages)
