;;; Minibuffer completion. -*- lexical-binding: t -*-

;; Fuzzy completion in the minibuffer. Uses `flx' by default, but can be configured to use the faster `flx-rs'.
(use-package fussy
  :ensure t
  :init
  (defun my-use-fussy-completion-in-minibuffer ()
    "Enable fussy completion in the minibuffer only."
    (setq-local completion-styles '(fussy)))
  (add-hook 'minibuffer-setup-hook #'my-use-fussy-completion-in-minibuffer)
  :config
  (setq fussy-use-cache t)) ; Reduce flicker when typing with lots of candidates.

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config

  ;; Hide the item count.
  (setq vertico-count-format nil)

  ;; Preselect the first candidate instead of the prompt. (You can always go back to the prompt by pressing C-p.)
  (setq vertico-preselect 'first)

  ;; Define a special keymap for directory browsing.
  (require 'vertico-directory)

  ;; C-u -- delete to the beginning of input, or go up a directory.
  (defun my-vertico-backward-kill-line ()
    (interactive)
    (if (= (minibuffer-prompt-end) (point))
        (vertico-directory-up)
      (delete-region (minibuffer-prompt-end) (point))))
  (bind-key "C-u" #'my-vertico-backward-kill-line vertico-directory-map)

  ;; Ivy-like RET behavior for directories.
  (bind-key "RET" #'vertico-directory-enter vertico-directory-map)

  ;; Ivy-like / behavior for directories (binding / straight to `vertico-directory-enter' would interfere with typing absolute paths and "~/".)
  (defun my-vertico-smart-slash ()
    "A wrapper for `vertico-directory-enter' that doesn't interfere with typing / or ~/."
    (interactive)
    (if (string-match-p "\\(?:^\\|/\\)~?$" (minibuffer-contents)) ;TODO: string-suffix-p "/" or "/~" maybe?
        (insert "/")
      (vertico-directory-enter)))
  (bind-key "/" #'my-vertico-smart-slash vertico-directory-map)

  ;; C-w, C-backspace -- delete the word/path before point
  (bind-key "C-w" #'vertico-directory-delete-word vertico-directory-map)
  (bind-key "<C-backspace>" #'vertico-directory-delete-word vertico-directory-map)

  ;; Clean up redundant path segments automatically, like Ivy.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (defun my-vertico-sort-files-by-mtime (files)
    "Sort files by modification time."
    (let ((dir default-directory))
      (if (file-remote-p dir)
          ;; Don't check mtime for remote files (too slow).
          (sort files #'string<)
        (let ((mtimes (make-hash-table :test 'equal)))
          ;; Precompute mtimes to reduce disk accesses.
          (dolist (file files)
            (puthash file (file-attribute-modification-time
                           (file-attributes (expand-file-name file dir)))
                     mtimes))
          (sort files (lambda (a b)
                        (time-less-p (gethash b mtimes)
                                     (gethash a mtimes))))))))

  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories
        ;; Files: sort by mtime, filter via fussy.
        '((file (styles fussy)
                (vertico-sort-override-function . my-vertico-sort-files-by-mtime)
                (:keymap . vertico-directory-map)))))

;; Show extra information next to completions.
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; Enable ubiquitous Vertico integration.
(use-package consult
  :ensure t
  :init

  ;; Go to function/variable/heading.
  (bind-key "C-x g" #'consult-imenu))

;; Embark - actions on minibuffer candidates.
(use-package embark
  :ensure t
  :init
  (with-eval-after-load 'vertico
    (bind-key "C-o" #'embark-act vertico-map))
  (setq prefix-help-command #'embark-prefix-help-command))

;; Better integration of Embark with Consult.
(use-package embark-consult
  :ensure t) ; We only need to install it. Embark will find it automatically.

;; Vertico's M-x relies on Emacs' native savehist-mode to sort by history and frequency.
(savehist-mode 1)

;; Insert LaTeX math symbol.
(defun my-insert-latex-math ()
  "Insert LaTeX math symbol."
  (interactive)
  (require 'latex)
  (let* ((math-list (append LaTeX-math-list LaTeX-math-default))
         (candidates (mapcar (lambda (item)
                               (let ((char (nth 0 item))
                                     (sym (nth 1 item)))
                                 (cons (if (listp sym) (nth 1 sym) sym) sym)))
                             math-list)))
    (insert "\\" (completing-read "LaTeX math: " candidates nil t))))
(bind-key "C-c i" #'my-insert-latex-math)

(provide 'conf/minibuffer/completion)
