;;; Close prompts with <escape>.

;; Don't use "ESC" instead of "<escape>":
;; "<escape>" is the event and "ESC" is the ASCII character the event is translated to (by default).

(global-unset-key (kbd "<escape>"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(mapc (lambda (keymap) (define-key keymap (kbd "<escape>") 'keyboard-escape-quit))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))

(provide 'conf/minibuffer/esc-quits-prompts)
