;;; Ace jump mode -- quickly jump to word/char.

(use-package ace-jump-mode
  :ensure t
  :defer t

  :init ; We define the motion here instead of in :config so that ace-jump-mode can be lazily loaded when `ace-jump-mode' is called.

  (with-eval-after-load 'evil
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

    (evil-define-motion evil-ace-jump-word (count)
      :type exclusive
      (evil-enclose-ace-jump
       (ace-jump-mode 1)))

    (bind-key "g SPC" #'evil-ace-jump-word evil-motion-state-map))

  :config

  ;; Use lowercase letters for jumping.
  (setq ace-jump-mode-move-keys (number-sequence ?a ?z))

  ;; Be case insensitive when choosing the [head] character.
  (setq ace-jump-mode-case-fold t))

(provide 'conf/moving/ace-jump)
