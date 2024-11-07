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

  ;; Remove bindings that could conflict with completion.
  ;; (YASnippet uses `yas-minor-mode-map' at all times, and `yas-keymap' immediately after expanding a snippet.)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (bind-key "TAB" nil keymap))

  ;; Use C-y and C-s to navigate between fields. When no snippet is active, expand a snippet with C-s.
  (with-eval-after-load 'evil
    (evil-define-key 'insert yas-minor-mode-map (kbd "C-s") #'yas-expand))
  (bind-key "C-s" #'yas-next-field yas-keymap)
  (bind-key "C-y" #'yas-prev-field yas-minor-mode-map) ; Even though navigating between fields makes sense only immediately after expanding a snippet (when `yas-keymap' is active), we place the binding in `yas-minor-mode-map' so that C-y errors out instead of starting isearch.

  ;; Indent snippets to the current column instead of using `indent-according-to-mode'. See <https://github.com/joaotavora/yasnippet/issues/485>.
  (setq yas-indent-line 'fixed))

(provide 'conf/editing/snippets)
