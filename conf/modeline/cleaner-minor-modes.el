;;; Change certain minor mode names.

(require 'conf/packages)
(package-ensure-installed 'diminish)

(defun diminish-some-modes ()
  "Change the modeline display of some minor modes.
This should be run after init, so that minor modes are already loaded."
  (diminish 'smartparens-mode "")
  (diminish 'undo-tree-mode "")
  (diminish 'page-break-lines-mode "")
  (diminish 'guide-key-mode ""))
(add-hook 'after-init-hook #'diminish-some-modes)

(provide 'conf/modeline/cleaner-minor-modes)
