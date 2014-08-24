;;; Snippets.

(require 'conf/packages)

(use-package yasnippet
  :ensure yasnippet
  :diminish yas-minor-mode
  :commands yas-global-mode
  :init
  (yas-global-mode 1)
  :config

  ;; Be less verbose.
  (setq yas-verbosity 1)

  ;; Remove YASnippet's bindings for expanding snippets (we'll use `auto-complete' and `completion-at-point' for that).
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)

  ;; Don't use TAB to cycle a snippet's fields.
  ;; (YASnippet uses `yas-minor-mode-map' at all times, and `yas-keymap' immediately after expanding a snippet.)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") nil)

  ;; Use C-y and C-s to navigate between fields. When no snippet is active, expand a snippet with C-s.
  (define-key yas-minor-mode-map (kbd "C-s") #'yas-expand)
  (define-key yas-keymap (kbd "C-s") #'yas-next-field)
  (define-key yas-minor-mode-map (kbd "C-y") #'yas-prev-field)) ; Even though navigating between fields makes sense only immediately after expanding a snippet (when `yas-keymap' is active), we place the binding in `yas-minor-mode-map' so that C-y errors out instead of starting isearch.

(provide 'conf/editing/snippets)
