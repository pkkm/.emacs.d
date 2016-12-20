;;; Snippets. -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config

  ;; Use snippets from ~/.emacs.d/snippets.
  ;; (YASnippet normally does this by default, but I set `user-emacs-directory' to something else than ~/.emacs.d.)
  (let ((snippets-dir (expand-file-name "snippets" main-dir)))
    (add-to-list 'yas-snippet-dirs snippets-dir)
    (yas-load-directory snippets-dir))

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
  (bind-key "TAB" nil yas-minor-mode-map)
  (bind-key "<tab>" nil yas-minor-mode-map)

  ;; Don't use TAB to cycle a snippet's fields.
  ;; (YASnippet uses `yas-minor-mode-map' at all times, and `yas-keymap' immediately after expanding a snippet.)
  (bind-key "TAB" nil yas-keymap)
  (bind-key "<tab>" nil yas-keymap)

  ;; Use C-y and C-s to navigate between fields. When no snippet is active, expand a snippet with C-s.
  (with-eval-after-load 'evil
    (evil-define-key 'insert yas-minor-mode-map (kbd "C-s") #'yas-expand))
  (bind-key "C-s" #'yas-next-field yas-keymap)
  (bind-key "C-y" #'yas-prev-field yas-minor-mode-map)) ; Even though navigating between fields makes sense only immediately after expanding a snippet (when `yas-keymap' is active), we place the binding in `yas-minor-mode-map' so that C-y errors out instead of starting isearch.

(provide 'conf/editing/snippets)





