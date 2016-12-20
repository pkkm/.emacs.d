;;; YAML (including Ansible files). -*- lexical-binding: t -*-

(use-package yaml-mode
  :ensure t)

(use-package ansible
  :ensure t
  :init
  (defun ansible-mode-maybe-enable ()
    (when (and (stringp (buffer-file-name))
               (string-match "\\(site\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)"
                             (buffer-file-name)))
      (ansible 1)))
  (with-eval-after-load 'yaml-mode
    (add-hook 'yaml-mode-hook #'ansible-mode-maybe-enable)))

(use-package ansible-doc
  :ensure t
  :init
  (with-eval-after-load 'ansible
    (add-hook 'ansible::hook #'ansible-doc-mode)))

(provide 'conf/mode-specific/yaml)
