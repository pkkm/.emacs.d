;;; Ace jump mode -- quickly jump to word/char.

(require 'conf/evil)

(define-key evil-motion-state-map (kbd "g SPC") #'evil-ace-jump-word)
(define-key evil-motion-state-map (kbd "g C-SPC") #'evil-ace-jump-char)
(global-set-key (kbd "C-X SPC") #'evil-ace-jump-line) ; Will be bound to "e SPC" by 'conf/other/make-prefix-keys-more-convenient.

(require 'conf/packages)
(package-ensure-installed 'ace-jump-mode)

;; Make the background gray when jumping.
(setq ace-jump-mode-gray-background t)

;; Use only lowercase letters for jumping.
(require 'cl-lib) ; Used: cl-loop.
(setq ace-jump-mode-move-keys
      (cl-loop for i from ?a to ?z collect i))

;; Be case insensitive when choosing the [head] character.
(setq ace-jump-mode-case-fold t)

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
   (ace-jump-mode 1)))

(provide 'conf/motions/ace-jump)
