;;; Puppet manifests.

(require 'conf/packages)
(package-ensure-installed 'puppet-mode)

;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-add-language-support puppet puppet-mode-hook
  ((puppet-indent-line . puppet-indent-level)))
(smart-tabs-insinuate 'puppet)
(add-hook 'puppet-mode-hook #'enable-indent-tabs-mode)

(provide 'conf/mode-specific/puppet)
