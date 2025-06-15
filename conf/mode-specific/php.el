;;; PHP. -*- lexical-binding: t -*-
;; The configuration is pretty slim because I do most things through LSP. Right now, I'm using phpactor because it's open source. Doom and lsp-mode docs both recommend intelephense, but it's freemium.
;; For phpactor configuration, see <https://emacs-lsp.github.io/lsp-mode/page/lsp-phpactor/>. For intelephense, see <https://emacs-lsp.github.io/lsp-mode/page/lsp-intelephense/> and <https://github.com/doomemacs/doomemacs/tree/master/modules/lang/php#im-missing-functionality-on-lsp-mode>.

(use-package php-mode
  :ensure t)

;; Add ignores for LSP file watchers.
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'") ; Composer's default package directory.
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]var[/\\\\]cache\\'")) ; Symfony cache.

;; For usage, see <https://github.com/nlamirault/phpunit.el>.
;; For running tests in docker-compose, see this code from Doom: <https://github.com/doomemacs/doomemacs/blob/ba1dca322f9a07bc2b7bec6a98f2c3c55c0bbd77/modules/lang/php/config.el#L153-L162>.
;; (use-package phpunit
;;   :ensure t)

;; Not used right now because I run composer commands from the shell.
;; Used by Doom's php layer with the following customizations:
;; - https://github.com/doomemacs/doomemacs/blob/ba1dca322f9a07bc2b7bec6a98f2c3c55c0bbd77/modules/lang/php/autoload.el#L3-L15
;; - https://github.com/doomemacs/doomemacs/blob/ba1dca322f9a07bc2b7bec6a98f2c3c55c0bbd77/modules/lang/php/config.el#L124-L139
;; (use-package composer
;;   :ensure t)

;; Also consider: <https://github.com/pivaldi/php-cs-fixer>.

;; Also consider: <https://github.com/emacs-php/psysh.el>, the PHP REPL used by Doom.

(provide 'conf/mode-specific/php)
