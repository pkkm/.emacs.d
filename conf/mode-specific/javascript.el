;;; Javascript.

;; Javascript IDE.
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (setq js2-highlight-level 3)) ; Highlight many built-in functions.

;; Skewer -- live web development minor mode.
;; Methods of launching:
;;   * M-x run-skewer
;;   * Use the Skewer bookmarklet to inject it into an existing page.
;; Keybindings resemble the Lisp ones:
;;   C-x C-e -- JS: eval form (with prefix: insert result); CSS: load declaration.
;;   C-M-x -- JS: eval top-level-form; CSS: load rule; HTML: load tag.
;;   C-c C-k -- JS, CSS: eval buffer.
;;   C-c C-z -- JS: switch to REPL (logging: "skewer.log()", like "console.log()").
;; Forms are sent to all attached clients simultaneously (use `list-skewer-clients' to show them).
;; If the browser disconnects, use "skewer()" in the browser console to reconnect.
(use-package skewer-mode
  :ensure t
  :init
  (skewer-setup)) ; Integrate with js2-mode, html-mode and css-mode. (Don't worry about performance, this function is in a separate file.)

;; Auto-complete support (also provides jump-to-definition).
(use-package ac-js2
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'ac-js2-mode)
  :config
  ;; C-c . -- jump to definition.
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap ac-js2-mode-map)
  (bind-key "C-c ." #'ac-js2-jump-to-definition ac-js2-mode-map)
  (bind-key "C-c ," #'pop-tag-mark ac-js2-mode-map)
  ;; C-c C-c -- expand function arguments.
  (with-eval-after-load 'yasnippet
    (bind-key "C-c C-c" #'ac-js2-expand-function)))

;; Refactoring (and many common operations, e.g. kill expression).
;; Integrates nicely with `emacs-refactor' (select some code before invoking it).
(use-package js2-refactor
  :ensure t
  :init
  ;; Load when js2-mode is turned on.
  (defun require-js2-refactor ()
    (require 'js2-refactor))
  (add-hook 'js2-mode-hook #'require-js2-refactor)
  :config
  ;; C-c C-m -- a prefix for many refactorings (e.g. ef -- extract function).
  (js2r-add-keybindings-with-prefix "C-c C-m"))
;; Doesn't work.
;; It seems that `smartparens' binds some characters to `sp--self-insert-command' in the menu buffer, which makes the menu items unselectable.
;; (use-package discover-js2-refactor
;;   :ensure t
;;   :init
;;   ;; Don't let `discover-js2-refactor's bind its default -- "C-c C-r". Instead, bind "C-c C-m" manually.
;;   (with-eval-after-load 'discover
;;     (with-eval-after-load 'js2-refactor
;;       (require 'discover-js2-refactor)
;;       (remove-hook 'js2-mode-hook #'js2-refactor-turn-on-discover)
;;       (with-eval-after-load 'js2-mode
;;         (bind-key "C-c C-m"
;;                   (discover-get-context-menu-command-name 'js2-refactor)
;;                   js2-mode-map)))))

(provide 'conf/mode-specific/javascript)
