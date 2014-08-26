;;; Package system.

;; Initialize packages now, instead of after init.
(package-initialize) ; This normally happens after loading the init file.
(setq package-enable-at-startup nil) ; Don't load the packages the second time after the init file.

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; To update installed packages, use M-x package-list-packages RET U x.
;; Or delete the elpa/ directory and launch Emacs for it to be recreated.

;; First time that `package-install' is called in this session, refresh the package list (if it wasn't already refreshed).
(defadvice package-install (before refresh-before-install activate)
  "Refresh the package list before installing a new package.
This will happen at most once per session, as this advice is removed when `package-refresh-contents' is called."
  (package-refresh-contents))
(defadvice package-refresh-contents (before disable-refresh-before-install activate)
  "Remove the advice to `package-install' that refreshes the package list, then self-destruct."
  (ad-remove-advice 'package-install 'before 'refresh-before-install)
  (ad-update 'package-install) ; Necessary for the above change to take effect.
  (ad-remove-advice 'package-refresh-contents 'before 'disable-refresh-before-install)
  (ad-update 'package-refresh-contents))

(defun package-ensure-installed (package)
  "Ensure the ELPA package PACKAGE is installed."
  (unless (package-installed-p package)
    (package-install package)))

;; Use-package, bind-key and diminish -- configure packages in a tidy, performance-oriented way.
(package-ensure-installed 'diminish)
(package-ensure-installed 'bind-key)
(autoload 'bind-key "bind-key")
(package-ensure-installed 'use-package)
(require 'use-package) ; If `use-package' is autoloaded, this file produces an error when its compiled version is loaded.

;; A replacement for `eval-after-load' that can depend on multiple packages/features.
;; When used with a single package, this works like the :config argument of `use-package'.
(autoload 'with-package "with-package") ; Installed in `my-vendor-dir'.

(provide 'conf/packages)
