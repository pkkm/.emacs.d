;;; Disable weird code from Debian's `pylint' package.
;; The package contains the file `/etc/emacs/site-start.d/50pylint.el', which adds bindings under `C-c m' in `python-mode'.

(remove-hook 'python-mode-hook #'pylint-add-menu-items)
(remove-hook 'python-mode-hook #'pylint-add-key-bindings)
(defun pylint-add-key-bindings (&rest _))

(provide 'conf/other/disable-pylint-weirdness)
