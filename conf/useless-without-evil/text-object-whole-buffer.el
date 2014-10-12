;;; "Whole buffer" text object.

(with-eval-after-load 'evil
  ;; ah -- whole buffer.
  (evil-define-text-object evil-whole-buffer (count &rest other-args)
    "Select the whole buffer."
    :type 'inclusive
    (list (point-min) (point-max)))
  (bind-key "h" #'evil-whole-buffer evil-outer-text-objects-map)

  ;; ih -- from the first to the last non-whitespace character.
  (evil-define-text-object evil-whole-buffer-without-whitespace (count &rest other-args)
    "Select from the first to the last non-whitespace character in the buffer.
Whitespace characters are \n and characters marked as whitespace in current syntax table.
If the buffer consists only of spaces, select the whole buffer."
    :type 'exclusive
    (let ((non-whitespace-regex "[^\n[:space:]]"))
      (save-excursion
        (let ((after-first-non-space
               (progn (goto-char (point-min))
                      (re-search-forward non-whitespace-regex nil t)))
              (after-last-non-space
               (progn (goto-char (point-max))
                      (re-search-backward non-whitespace-regex nil t))))
          (if after-first-non-space ; If the buffer contains non-whitespace characters...
              (list (- after-first-non-space 1) after-last-non-space)
            (list (point-min) (point-max)))))))
  (bind-key "h" #'evil-whole-buffer-without-whitespace evil-inner-text-objects-map))

(provide 'conf/useless-without-evil/text-object-whole-buffer)
