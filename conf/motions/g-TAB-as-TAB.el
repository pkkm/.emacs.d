;;; Make "g TAB" do what "TAB" would normally do (in the Emacs state) in the current major mode.

(require 'conf/evil)

(defun current-major-mode-TAB ()
  "Do what the TAB key would normally do in the current major mode (`current-local-map')."
  (interactive)
  (let ((major-mode-TAB-binding
         (lookup-key (current-local-map) (kbd "TAB"))))
    (when major-mode-TAB-binding
      (call-interactively major-mode-TAB-binding))))

(define-key evil-motion-state-map (kbd "g TAB") #'current-major-mode-TAB)

(provide 'conf/motions/g-TAB-as-TAB)
