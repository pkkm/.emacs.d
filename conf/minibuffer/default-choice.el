;;; Default choice in minibuffer prompts.

;; Shorten "(default ...)" to "[...]".
(setq minibuffer-eldef-shorten-default t)

;; Show the default only when hitting RET would yield it.
(minibuffer-electric-default-mode 1)

(provide 'conf/minibuffer/default-choice)
