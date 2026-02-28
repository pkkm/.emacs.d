;;; Minibuffer completion. -*- lexical-binding: t -*-

;; Useful ido-mode default keybindings:
;;   * C-k -- delete current match (works for buffers, files, ...)
;;   * C-SPC -- search in the current set of results

(use-package ido ; Bundled with Emacs.
  :init
  (ido-mode 1)
  :config

  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess) ; If there's a filename at point, start with it filled in.
  (setq ido-auto-merge-work-directories-length -1) ; Don't automaticaly search other directories for the typed file name.

  ;; Ido provides the keymaps `ido-common-completion-map', `ido-file-dir-completion-map', `ido-file-completion-map', `ido-buffer-completion-map' for various kinds of completions.
  ;; However, it recreates them every time `ido-completing-read' is called, so we need to define custom keys every time too.
  (defun my-ido-bindings ()
    ;; `ido-completion-map' -- the current completion keymap.
    ;; `ido-cur-item' -- the type of item that is being read: file, dir, buffer or list.

    ;; Don't complete on SPC.
    (bind-key "SPC" nil ido-completion-map)

    ;; C-n, C-p -- cycle matches.
    (bind-key "C-n" #'ido-next-match ido-completion-map)
    (bind-key "C-p" #'ido-prev-match ido-completion-map)

    ;; C-w, C-backspace -- delete the word before point.
    (if (memq ido-cur-item '(file dir))
        (progn
          (bind-key "C-w" #'ido-delete-backward-word-updir ido-completion-map)
          (bind-key "<C-backspace>" #'ido-delete-backward-word-updir ido-completion-map))
      (bind-key "C-w" #'backward-kill-word ido-completion-map))

    ;; C-u -- delete to the beginning of input.
    (if (memq ido-cur-item '(file dir))
        (bind-key "C-u" #'ido-delete-backward-line-updir ido-completion-map)
      (bind-key "C-u" #'my-backward-kill-line ido-completion-map)))
  (add-hook 'ido-setup-hook #'my-ido-bindings) ; Run on every completion after keymaps have been set up.

  ;; Function for C-u.
  (defun my-backward-kill-line ()
    (interactive)
    (kill-line 0))
  (defun ido-delete-backward-line-updir ()
    (interactive)
    (if (= (minibuffer-prompt-end) (point))
        (ido-up-directory t)
      (my-backward-kill-line))))

;; Display completions vertically.
(use-package ido-vertical-mode
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-vertical-mode)))

;; Use Ido almost everywhere.
(use-package ido-completing-read+
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-ubiquitous-mode 1))
  :config
  (setq ido-cr+-max-items 100000)) ; Default is 30k, insert-char needs about 40k.

;; Better, more memory-hungry flex matching.
(use-package flx-ido
  :ensure t
  :init
  (with-eval-after-load 'ido
    (flx-ido-mode 1)))

;; Sort Ido's file list by modification time.
(use-package ido-sort-mtime
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-sort-mtime-mode 1))
  :config
  (setq ido-sort-mtime-tramp-files-at-end t))

;; Extended Ido for M-x.
;; Interesting command: amx-show-unbound-commands -- show frequently called commands that are unbound.
(use-package amx
  :ensure t
  :bind (("M-x" . amx)
         ("C-x SPC" . amx)))

;; Helm -- an alternative to ido-mode with more features, but no flx matching.
(use-package helm
  :ensure t
  :init

  ;; Go to some function/variable definition (works with a lot of modes).
  (bind-key "C-x g" #'helm-semantic-or-imenu)

  ;; Insert LaTeX math symbol.
  (bind-key "C-c i" #'helm-insert-latex-math)
  (defun my-require-auctex-for-helm-insert-latex-math (&rest _args)
    (require 'latex)) ; Require the needed part of AUCTeX (otherwise the function will error out).
  (advice-add 'helm-insert-latex-math :before #'my-require-auctex-for-helm-insert-latex-math))

(provide 'conf/minibuffer/completion)
