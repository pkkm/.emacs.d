;;; Ace jump mode -- quickly jump to word/char.

(use-package ace-jump-mode
  :ensure t
  :defer t
  :init

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
      (require 'ace-jump-mode)
      (evil-enclose-ace-jump
       (ace-jump-mode 1)))

    (bind-key "g SPC" #'evil-ace-jump-word evil-motion-state-map)))

(provide 'conf/moving/ace-jump)
