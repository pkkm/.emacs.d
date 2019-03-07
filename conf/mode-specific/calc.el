;;; Customization of Emacs calculator mode. -*- lexical-binding: t -*-

;; Use the `--calc' argument to start Emacs Calc-only.
(defun my-calc-after-startup (&rest ignored-args)
  "Open `calc' in fullscreen after Emacs startup."
  (add-hook 'emacs-startup-hook #'full-calc t))
(add-to-list 'command-switch-alist (cons "--calc" #'my-calc-after-startup))

(use-package calc
  :config

  ;; Don't give multiplication higher precedence than division.
  (setq calc-multiplication-has-precedence nil)

  ;; Cheatsheet.
  (defun my-cheatsheet-calc ()
    (interactive)
    (open-cheatsheet "https://www.gnu.org/software/emacs/refcards/pdf/calccard.pdf")) ; From conf/other/cheatsheets.
  (bind-key "C-c C" #'my-cheatsheet-calc calc-mode-map))

(provide 'conf/mode-specific/calc)
