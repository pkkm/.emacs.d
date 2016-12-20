;;; Spell checking. -*- lexical-binding: t -*-

(use-package flyspell ; Included in Emacs.
  :diminish "FlyS" ; Default: "Fly".
  :config

  ;; Make a correct flyspell-mode hook (`flyspell-mode-hook' runs on both enable and disable; TODO bug report?).
  (defun run-my-flyspell-mode-hook ()
    (when flyspell-mode
      (run-hooks 'my-flyspell-mode-hook)))
  (add-hook 'flyspell-mode-hook #'run-my-flyspell-mode-hook)

  ;; Check whole buffer after turning Flyspell on.
  (add-hook 'my-flyspell-mode-hook #'flyspell-buffer))

(provide 'conf/driving-processes/spell-check)
