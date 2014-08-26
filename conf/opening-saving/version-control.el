;;; Version control interfaces.


;;; Magit (git interface).

(use-package magit
  :ensure magit
  :diminish magit-auto-revert-mode
  :commands magit-status
  :bind ("C-c g" . magit-status))


;;; Disable VC-mode (the default Emacs interface for VCSes).
(setq vc-handled-backends '())


(provide 'conf/opening-saving/version-control)
