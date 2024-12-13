;;; Puppet manifests. -*- lexical-binding: t -*-

(use-package puppet-mode
  :ensure t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support puppet puppet-mode-hook
      ((puppet-indent-line . puppet-indent-level)))
    (smart-tabs-insinuate 'puppet)
    (add-hook 'puppet-mode-hook #'enable-indent-tabs-mode)

    ;; This used not to be necessary before I updated to Emacs 28 and newer packages. I don't know why it's necessary now. It seems that other people don't either - see <https://github.com/jcsalomon/smarttabs/issues/51>.
    (setq puppet-indent-level tab-width)))

(provide 'conf/mode-specific/puppet)
