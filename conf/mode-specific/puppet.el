;;; Puppet manifests.

(use-package puppet-mode
  :ensure puppet-mode
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support puppet puppet-mode-hook
      ((puppet-indent-line . puppet-indent-level)))
    (smart-tabs-insinuate 'puppet)
    (add-hook 'puppet-mode-hook #'enable-indent-tabs-mode)))

(provide 'conf/mode-specific/puppet)
