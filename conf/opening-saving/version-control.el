;;; Version control interfaces.

;; Use Magit.
(require 'conf/packages)
(package-ensure-installed 'magit)

;; Disable VC-mode (the default Emacs interface for VCSe
(setq vc-handled-backends '())

(provide 'conf/opening-saving/version-control)
