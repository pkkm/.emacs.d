;;; Minibuffer completion. -*- lexical-binding: t -*-

;; Useful Ivy keybindings:
;;   * C-c C-k -- delete current match (e.g. kill buffer)
;;   * S-SPC -- search in the current set of results

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)

  :config

  ;; Only move home after "~/" rather than a bare "~".
  (setq ivy-magic-tilde nil)

  ;; Don't add "." and ".." to the file list.
  (setq ivy-extra-directories nil)

  ;; Make RET enter a directory instead of opening it in Dired.
  (bind-key "RET" #'ivy-alt-done ivy-minibuffer-map)

  ;; Highlight the entire line with the selection instead of just the text.
  (setq ivy-format-functions-alist '((t . ivy-format-function-arrow-line)))

  (setq ivy-count-format "")

  (setq ivy-sort-max-size 100000)

  ;; C-u -- delete to the beginning of input, or go up a directory if at the end of the prompt.
  (defun my-ivy-backward-kill-line ()
    (interactive)
    (if (= (minibuffer-prompt-end) (point))
        (ivy-backward-kill-word) ; Goes up a directory in Ivy file completion.
      (delete-region (minibuffer-prompt-end) (point))))
  (bind-key "C-u" #'my-ivy-backward-kill-line ivy-minibuffer-map)

  ;; C-w, C-backspace -- delete the word before point, or go up a directory.
  (bind-key "C-w" #'ivy-backward-kill-word ivy-minibuffer-map)
  (bind-key "<C-backspace>" #'ivy-backward-kill-word ivy-minibuffer-map)

  ;; Sort Ivy's file list by modification time.
  (defun my-ivy-compare-files-by-mtime (a b)
    "Sort files by modification time. Put remote files at the end without checking their modification time."
    (let* ((dir (or ivy--directory default-directory))
           ;; Use `concat' instead of `expand-file-name' so that Emacs doesn't try to access the remote file system.
           (a-is-remote (string-match-p tramp-file-name-regexp (concat dir a)))
           (b-is-remote (string-match-p tramp-file-name-regexp (concat dir b))))
      (cond
       ((and a-is-remote b-is-remote) (string< a b))
       (a-is-remote nil)
       (b-is-remote t)
       (t (file-newer-than-file-p (expand-file-name a dir) (expand-file-name b dir))))))
  (add-to-list 'ivy-sort-functions-alist '(read-file-name-internal . my-ivy-compare-files-by-mtime))

  ;; Insert LaTeX math symbol.
  (defun my-ivy-insert-latex-math ()
    "Insert LaTeX math symbol using Ivy."
    (interactive)
    (require 'latex)
    (let* ((math-list (append LaTeX-math-list LaTeX-math-default))
           (candidates (mapcar (lambda (item)
                                 (let ((char (nth 0 item))
                                       (sym (nth 1 item)))
                                   (cons (if (listp sym) (nth 1 sym) sym) sym)))
                               math-list)))
      (ivy-read "LaTeX math: " (mapcar #'car candidates)
                :require-match t
                :action (lambda (x) (insert "\\" x)))))
  (bind-key "C-c i" #'my-ivy-insert-latex-math))

;; Better but slower flex matching.
(use-package flx
  :ensure t
  :init
  (with-eval-after-load 'ivy
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

;; Enable ubiquitous Ivy integration.
(use-package counsel
  :ensure t
  :diminish counsel-mode
  :init
  (counsel-mode 1)

  ;; Go to function/variable/heading.
  (bind-key "C-x g" #'counsel-imenu))

;; Some extra features for M-x.
;; Interesting command: amx-show-unbound-commands -- show frequently called commands that are unbound.
(use-package amx
  :ensure t
  :bind (("M-x" . amx)
         ("C-x SPC" . amx)))

;; Add extra information for some commands, e.g. buffer switching.
(use-package ivy-rich
  :ensure t
  :init
  (with-eval-after-load 'counsel
    (ivy-rich-mode 1)))

(provide 'conf/minibuffer/completion)
