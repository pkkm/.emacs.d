;;; Snippets.

(require 'conf/packages)
(require 'conf/evil)

(use-package yasnippet
  :ensure yasnippet
  :diminish yas-minor-mode
  :commands yas-global-mode
  :init
  (yas-global-mode 1)
  :config

  ;; Be less verbose.
  (setq yas-verbosity 1)

  ;; Make `completion-at-point' try to expand a snippet before attempting any other completions.
  (defun yas-expand-if-active () ; If this function name is changed, also change it in the code for YASnippet integration in conf/editing/completion.el.
    "Call `yas-expand' if YASnippet minor mode is active."
    (when yas-minor-mode
      (yas-expand)))
  (setq yas-fallback-behavior 'return-nil) ; So that `completion-at-point' tries the next function instead of stopping.
  (defun add-yas-expand-to-completion-at-point ()
    (add-to-list 'completion-at-point-functions #'yas-expand-if-active))
  (add-hook 'yas-minor-mode-hook #'add-yas-expand-to-completion-at-point)

  ;; Remove YASnippet's bindings for expanding snippets (we'll use `auto-complete' and `completion-at-point' for that).
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)

  ;; Don't use TAB to cycle a snippet's fields.
  ;; (YASnippet uses `yas-minor-mode-map' at all times, and `yas-keymap' immediately after expanding a snippet.)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap (kbd "<tab>") nil)

  ;; Use C-y and C-s to navigate between fields. When no snippet is active, expand a snippet with C-s.
  (evil-define-key 'insert yas-minor-mode-map (kbd "C-s") #'yas-expand)
  (define-key yas-keymap (kbd "C-s") #'yas-next-field)
  (define-key yas-minor-mode-map (kbd "C-y") #'yas-prev-field)) ; Even though navigating between fields makes sense only immediately after expanding a snippet (when `yas-keymap' is active), we place the binding in `yas-minor-mode-map' so that C-y errors out instead of starting isearch.

(provide 'conf/editing/snippets)
