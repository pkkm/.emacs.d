;;; Jumping to mark/register. -*- lexical-binding: t -*-
;; Emacs registers are global, in contrast to Evil's buffer-local markers. They also can be used for storing window configurations, file names, etc.

(with-eval-after-load 'evil
  ;; ' -- go to a mark (instead of the mark's line).
  (evil-define-key 'motion 'global (kbd "'") #'evil-goto-mark)

  ;; M -- store the point position in an Emacs register
  ;; ` -- jump to a register.
  (evil-define-key 'motion 'global (kbd "M") #'point-to-register)
  (evil-define-key 'motion 'global (kbd "`") #'jump-to-register))

(provide 'conf/evil-specific/marks-registers)
