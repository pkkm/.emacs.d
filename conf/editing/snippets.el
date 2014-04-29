;;; Snippets.

(require 'conf/packages)
(package-ensure-installed 'yasnippet)

(yas-global-mode 1)

;; Remove YASnippet's bindings for expanding snippets (we'll use `auto-complete' and `completion-at-point' for that).
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)

;; After expanding a snippet, use C-n and C-p instead of TAB to cycle its fields.
;; (YASnippet uses `yas-minor-mode-map' at all times, and `yas-keymap' immediately after expanding a snippet.)
(define-key yas-keymap (kbd "TAB") nil)
(define-key yas-keymap (kbd "<tab>") nil)
(require 'conf/evil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key yas-keymap (kbd "C-p") #'yas-prev-field)
(define-key yas-keymap (kbd "C-n") #'yas-next-field)

;; Be less verbose.
(setq yas-verbosity 1)

;; Don't show YASnippet in the mode line.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'yas-minor-mode)

(provide 'conf/editing/snippets)
