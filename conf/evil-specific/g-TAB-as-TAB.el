;;; Make "g TAB" do what "TAB" would normally do in the current major mode. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (defun current-major-mode-TAB ()
    "Do what the TAB key would normally do in the current major mode (`current-local-map')."
    (interactive)
    (let ((major-mode-TAB-binding
           (lookup-key (current-local-map) (kbd "TAB"))))
      (when major-mode-TAB-binding
        (call-interactively major-mode-TAB-binding))))
  (evil-define-key 'motion 'global (kbd "g TAB") #'current-major-mode-TAB))

(provide 'conf/evil-specific/g-TAB-as-TAB)
