;;; Configuration for web-mode -- a mode for mixed HTML, CSS and PHP.

(use-package web-mode
  :ensure web-mode
  :defer t
  :init

  ;; Use web-mode for HTML and PHP files.
  (dolist (extension '("htm" "html" "php"))
    (add-to-list 'auto-mode-alist
                 (cons (concat "\\." extension "\\'") 'web-mode)))

  :config


  ;;; Indentation.

  ;; Use Smart Tabs.
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support web web-mode-hook
      ((web-mode-indent-line . web-mode-code-indent-offset)))
    (smart-tabs-insinuate 'web)
    (add-hook 'web-mode-hook #'enable-indent-tabs-mode))

  ;; Make all indentation offsets equal.
  ;; (This is needed by Smart Tabs -- `smart-tabs-add-language-support' can take only one variable name per function name.)
  (defvaralias 'web-mode-markup-indent-offset 'web-mode-code-indent-offset)
  (defvaralias 'web-mode-css-indent-offset 'web-mode-code-indent-offset)
  (defvaralias 'web-mode-sql-indent-offset 'web-mode-code-indent-offset)

  ;; Don't indent non-HTML blocks with additional spaces (there's no option for tabs).
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  (setq web-mode-block-padding 0)


  ;; Completion.
  (with-eval-after-load 'auto-complete
    ;; Web mode has an alist for auto-complete sources (if it's not set, auto-complete won't work).
    (setq web-mode-ac-sources-alist
          `(("php" . ,(default-value 'ac-sources))
            ("html" . ,(default-value 'ac-sources))
            ("css" . (ac-source-css-property ,@(default-value 'ac-sources))))))

  ;; Use YASnippet snippets of other modes.
  ;; (An alternative approach would be to create "~/.emacs.d/snippets/web-mode/.yas-parents" with the mode names.)
  (with-eval-after-load 'yasnippet
    (defun my-web-mode-additional-yasnippets ()
      "If YASnippet is active, activate snippets of other modes that will be useful in web-mode."
      (dolist (mode '(css-mode html-mode))
        (yas-activate-extra-mode mode)))
    (add-hook 'web-mode-hook #'my-web-mode-additional-yasnippets))

  ;; Expand abbreviations like "p>ul>li*5" with C-j.
  (require 'conf/editing/emmet)
  (add-hook 'web-mode-hook #'emmet-mode))

(provide 'conf/mode-specific/web)
