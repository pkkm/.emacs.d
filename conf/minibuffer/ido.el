;;; Ido (Interactively DO) mode -- powerful completion in the minibuffer.
;;; This file is for general Ido settings -- for buffer, etc.-specific, see the respective files.

(use-package ido ; Bundled with Emacs.
  :demand t
  :config

  (ido-mode 1)

  ;; Flex matching.
  (setq ido-enable-flex-matching t)

  ;; Savefile.
  (setq ido-save-directory-list-file
        (expand-file-name "ido" my-savefile-dir))


  ;;; Keybindings.

  ;; Ido provides the keymaps `ido-common-completion-map', `ido-file-dir-completion-map', `ido-file-completion-map', `ido-buffer-completion-map' for various kinds of completions.
  ;; However, it recreates them every time `ido-completing-read' is called, so we need to define custom keys every time too.
  (defun my-ido-bindings ()
    ;; `ido-completion-map' -- the current completion keymap.
    ;; `ido-cur-item' -- the type of item that is being read: file, dir, buffer or list.

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
      (bind-key "C-u" #'backward-kill-line ido-completion-map)))
  (add-hook 'ido-setup-hook #'my-ido-bindings) ; Run on every completion after keymaps have been set up.

  ;; Function for C-u.
  (defun backward-kill-line ()
    (interactive)
    (kill-line 0))
  (defun ido-delete-backward-line-updir ()
    (interactive)
    (if (= (minibuffer-prompt-end) (point))
        (ido-up-directory t)
      (backward-kill-line)))

  ;; Make SPC insert a space (I haven't found any less hackish way to do this).
  (defalias 'ido-complete-space 'self-insert-command))


;; Display completions vertically.
(use-package ido-vertical-mode
  :ensure ido-vertical-mode
  :commands ido-vertical-mode
  :init (ido-vertical-mode)
  :pre-load
  ;; For some reason, this needs to happen before `ido-vertical-mode' is loaded.
  (setq ido-vertical-decorations
        '("\n- " ; Left bracket around prospect list.
          "" ; Right bracket around prospect list.
          "\n  " ; Separator between prospects.
          "\n  ..." ; Inserted at the end of a truncated list of prospects.
          "[" ; Left bracket around common match string.
          "]" ; Right bracket around common match string.
          " [No match]"
          " [Matched]"
          " [Not readable]"
          " [Too big]"
          " [Confirm]")))

;; Use Ido almost everywhere.
(use-package ido-ubiquitous
  :ensure ido-ubiquitous
  :commands ido-ubiquitous-mode
  :init (ido-ubiquitous-mode 1))

;; Better, more memory-hungry flex matching (used only on high-end machines).
(use-package flx-ido
  :ensure flx-ido
  :if (not (eq (getenv "LOW_END_MACHINE") "true"))
  :commands flx-ido-mode
  :init (flx-ido-mode 1))

(provide 'conf/minibuffer/ido)
