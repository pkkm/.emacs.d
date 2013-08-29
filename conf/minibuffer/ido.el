;;; Ido (Interactively DO) mode -- powerful completion in the minibuffer.
;;; This file is for general Ido settings -- for buffer, etc.-specific, see the respective files.

(ido-mode 1) ; Bundled package.

;; Use Ido almost everywhere.
(require 'conf/packages)
(package-ensure-installed 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Ido-hacks used to be here, but it was breaking my C-n binding (for #'ido-next-match).

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
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") #'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") #'ido-prev-match))
(add-hook 'ido-setup-hook #'ido-define-keys)

(provide 'conf/minibuffer/ido)
