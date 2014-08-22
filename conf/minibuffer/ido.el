;;; Ido (Interactively DO) mode -- powerful completion in the minibuffer.
;;; This file is for general Ido settings -- for buffer, etc.-specific, see the respective files.

(require 'conf/packages)
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

(use-package ido ; Bundled with Emacs.
  :demand t
  :config

  (ido-mode 1)

  ;; Flex matching.
  (setq ido-enable-flex-matching t)

  ;; Savefile.
  (setq ido-save-directory-list-file
        (expand-file-name "ido" my-savefile-dir))

  ;; Display completions vertically.
  (setq ido-decorations '("\n- "
                          ""
                          "\n  "
                          "\n  ..."
                          "["
                          "]"
                          " [No match]"
                          " [Matched]"
                          " [Not readable]"
                          " [Too big]"
                          " [Confirm]"))
  (defun disable-line-truncation ()
    "Disable line truncation in the current buffer.
This is needed for the vertical displaying of Ido completions to work."
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook #'disable-line-truncation)

  ;; Keybindings.
  (defun my-ido-bindings ()
    ;; Ido keymaps:
    ;;   * ido-common-completion-map -- keymap for all Ido commands
    ;;   * ido-completion-map -- currently active keymap for Ido.
    ;;   * ido-file-dir-completion-map, ido-file-completion-map
    ;;   * ido-buffer-completion-map

    ;; C-n, C-p -- cycle matches.
    (bind-key "C-n" #'ido-next-match ido-common-completion-map)
    (bind-key "C-p" #'ido-prev-match ido-common-completion-map)

    ;; SPC -- insert space (I haven't found any less hackish way to do this).
    (defalias 'ido-complete-space 'self-insert-command)

    ;; C-w, C-backspace -- delete the word before point.
    (bind-key "C-w" #'backward-kill-word ido-common-completion-map)
    (bind-key "C-w" #'ido-delete-backward-word-updir ido-file-completion-map)
    (bind-key "C-w" #'ido-delete-backward-word-updir ido-file-dir-completion-map)
    (bind-key "<C-backspace>" #'ido-delete-backward-word-updir ido-file-dir-completion-map)

    ;; C-u -- delete to the beginning of input.
    (bind-key "C-u" #'backward-kill-line ido-common-completion-map)
    (bind-key "C-u" #'ido-delete-backward-line-updir ido-file-dir-completion-map)
    (defun backward-kill-line ()
      (interactive)
      (kill-line 0))
    (defun ido-delete-backward-line-updir ()
      (interactive)
      (if (= (minibuffer-prompt-end) (point))
          (ido-up-directory t)
        (backward-kill-line))))
  (add-one-shot-hook 'ido-setup-hook #'my-ido-bindings))

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
