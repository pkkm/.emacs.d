;;; Ido (Interactively DO) mode -- powerful completion in the minibuffer.
;;; This file is for general Ido settings -- for buffer, etc.-specific, see the respective files.

(require 'conf/packages)
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

(use-package ido ; Bundled with Emacs.
  :demand t
  :config

  (ido-mode 1)

  ;; Use Ido almost everywhere.
  (ido-ubiquitous-mode)

  ;; Flex matching.
  (setq ido-enable-flex-matching t)
  ;; On high-end machines, use a better, more memory-hungry algorithm.
  (unless (eq (getenv "LOW_END_MACHINE") "true")
    (flx-ido-mode 1))

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
  (add-one-shot-hook 'ido-setup-hook #'my-ido-bindings))

(use-package ido-ubiquitous
  :ensure ido-ubiquitous
  :commands ido-ubiquitous-mode)

(use-package flx-ido
  :ensure flx-ido
  :commands flx-ido-mode)

(provide 'conf/minibuffer/ido)
