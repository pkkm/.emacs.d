;;; Spell checking. -*- lexical-binding: t -*-

(use-package flyspell ; Included in Emacs.
  :diminish "FlyS" ; Default: "Fly".
  :config

  ;; Check whole buffer after turning Flyspell on.
  (defun my-flyspell-check-buffer-on-enable ()
    (when flyspell-mode
      (flyspell-buffer)))
  (add-hook 'flyspell-mode-hook #'my-flyspell-check-buffer-on-enable))

(provide 'conf/driving-processes/spell-check)
