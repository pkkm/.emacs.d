;;; Version control interfaces.


;;; Magit (git interface).

(require 'conf/packages)
(package-ensure-installed 'magit)

;; Don't show magit-auto-revert-mode in the modeline.
;; (This minor mode reverts unmodified buffers affected by git commands.)
(require 'conf/utils/hooks)
(add-one-shot-hooks '(magit-mode-hook magit-auto-revert-mode-hook)
                    #'diminish-magit-auto-revert-mode)
(defun diminish-magit-auto-revert-mode ()
  (require 'conf/modeline/cleaner-minor-modes)
  (diminish 'magit-auto-revert-mode))


;;; Disable VC-mode (the default Emacs interface for VCSes).
(setq vc-handled-backends '())


(provide 'conf/opening-saving/version-control)
