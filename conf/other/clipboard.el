;;; Integration with the system clipboard(s).
;; FIXME: this breaks evil's C-p/C-n kill ring cycling.

;;; X Window System: Use PRIMARY for visual selection and CLIPBOARD for explicit cuts.
(when (eq window-system 'x)
  ;; Automatically copy the visual selection to PRIMARY (don't add to kill-ring).
  (setq x-select-enable-primary t
        x-select-enable-clipboard nil
        mouse-drag-copy-region nil)

  ;; On explicit yank, copy to CLIPBOARD.
  (defun x-copy-to-clipboard (text)
    "Copy TEXT to X's clipboard."
    (let ((x-select-enable-clipboard t)
          (x-select-enable-primary nil))
      (x-select-text text)))
  (setq interprogram-cut-function #'x-copy-to-clipboard)

  ;; When pasting, check the CLIPBOARD and if there's something new, insert it into the kill-ring.
  ;; Middle mouse button will still paste from PRIMARY.
  (defun x-paste-from-clipboard ()
    "If another program provided text for pasting, return it (as a string).
If the last paste was from Emacs or is identical to the last one from Emacs, return nil."
    (unless (string= (car kill-ring) (x-get-selection 'CLIPBOARD))
      (x-get-selection 'CLIPBOARD))) ; TODO maybe remove text properties?
  (setq interprogram-paste-function #'x-paste-from-clipboard))

;;; Terminal: Use the `xclip' utility to access CLIPBOARD on explicit cuts. Don't use PRIMARY.
(when (and (not (display-graphic-p))
           (getenv "DISPLAY")
           (executable-find "xclip"))
  ;; On explicit yank, copy to CLIPBOARD.
  (defun xclip-copy-to-clipboard (text)
    "Copy TEXT to X's clipboard, using the `xclip' utility."
    (let* ((process-connection-type nil)
           (proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
      (process-send-string proc text)
      (process-send-eof proc)))
  (setq interprogram-cut-function #'xclip-copy-to-clipboard)

  ;; When pasting, check CLIPBOARD.
  (defun xclip-paste-from-clipboard ()
    "If another program provided text for pasting, return it (as a string).
If the last paste was from Emacs or is identical to the last one from Emacs, return nil.
Uses the `xclip' utility."
    (let ((clipboard-contents (shell-command-to-string "xclip -o -selection clipboard")))
      (unless (string= (car kill-ring) clipboard-contents)
        clipboard-contents)))
  (setq interprogram-paste-function #'xclip-paste-from-clipboard))

;;; Windows, etc.: don't change the default.

(provide 'conf/other/clipboard)
