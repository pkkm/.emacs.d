;;; Ace jump mode -- quickly jump to word/char.

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :commands ace-jump-mode

  :init

  (with-eval-after-load 'evil
    ;; Keybindings.
    (bind-key "g SPC" #'evil-ace-jump-word evil-motion-state-map)
    (bind-key "g C-SPC" #'evil-ace-jump-char evil-motion-state-map)
    (bind-key "C-x SPC" #'evil-ace-jump-line) ; Will be bound to "e SPC" by 'conf/other/make-prefix-keys-more-convenient.

    (defmacro evil-enclose-ace-jump (&rest body)
      `(let ((old-mark (mark))
             (ace-jump-mode-scope 'window)
             (ace-jump-mode-end-hook ace-jump-mode-end-hook))
         (add-hook 'ace-jump-mode-end-hook #'exit-recursive-edit)
         (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
         (remove-hook 'post-command-hook #'evil-visual-post-command t)
         (unwind-protect
             (progn
               ,@body
               (recursive-edit))
           (if (evil-visual-state-p)
               (progn
                 (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
                 (add-hook 'post-command-hook #'evil-visual-post-command nil t)
                 (set-mark old-mark))
             (push-mark old-mark)))))

    (evil-define-motion evil-ace-jump-char (count)
      :type exclusive
      (evil-enclose-ace-jump
       (ace-jump-mode 5)))

    (evil-define-motion evil-ace-jump-line (count)
      :type line
      (evil-enclose-ace-jump
       (ace-jump-mode 9)))

    (evil-define-motion evil-ace-jump-word (count)
      :type exclusive
      (evil-enclose-ace-jump
       (ace-jump-mode 1))))

  :config

  ;; Use only lowercase letters for jumping.
  (setq ace-jump-mode-move-keys (number-sequence ?a ?z))

  ;; Be case insensitive when choosing the [head] character.
  (setq ace-jump-mode-case-fold t))

(provide 'conf/motions/ace-jump)
