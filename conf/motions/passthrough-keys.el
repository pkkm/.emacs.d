;;; Keys that invoke the major mode's binding.

;; Make "g TAB" do what "TAB" would normally do in the current major mode.
(with-eval-after-load 'evil
  (defun current-major-mode-TAB ()
    "Do what the TAB key would normally do in the current major mode (`current-local-map')."
    (interactive)
    (let ((major-mode-TAB-binding
           (lookup-key (current-local-map) (kbd "TAB"))))
      (when major-mode-TAB-binding
        (call-interactively major-mode-TAB-binding))))
  (bind-key "g TAB" #'current-major-mode-TAB evil-motion-state-map))

(provide 'conf/motions/passthrough-keys)
