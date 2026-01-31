;;; Configuration for web-mode -- a mode for mixed HTML, CSS, PHP and Javascript. -*- lexical-binding: t -*-

(use-package web-mode
  :ensure t
  :init

  ;; Use web-mode for HTML files.
  (dolist (extension '("htm" "html" "html.twig")) ; This used to contain "php" too, but I use php-mode now.
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


  ;; Use YASnippet snippets of other modes.
  ;; (An alternative approach would be to create "~/.emacs.d/snippets/web-mode/.yas-parents" with the mode names.)
  (with-eval-after-load 'yasnippet
    (defun my-web-mode-additional-yasnippets ()
      "If YASnippet is active, activate snippets for other modes that will be useful in web-mode."
      (dolist (mode '(css-mode html-mode))
        (yas-activate-extra-mode mode)))
    (add-hook 'web-mode-hook #'my-web-mode-additional-yasnippets))

  ;; Expand abbreviations like "p>ul>li*5" with C-j.
  (require 'conf/editing/emmet)
  (add-hook 'web-mode-hook #'emmet-mode))

(provide 'conf/mode-specific/web)
