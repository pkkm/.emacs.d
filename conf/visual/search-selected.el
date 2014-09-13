;;; Search for the selected text.

(defun search-text-between (begin end forward-p)
  "Search for the text between BEGIN and END (character offsets), forward if FORWARD-P is t."
  (let ((text-to-search (buffer-substring-no-properties begin end)))
    (setq isearch-forward forward-p) ; Tell ISearch whether the last search was forward or backward.
    (evil-search text-to-search forward-p)))

(defun search-selection-forward (sel-begin sel-end)
  "Search for the selected text forward."
  (interactive "r") ; Needs a selection.
  (deactivate-mark)
  (search-text-between sel-begin sel-end t))

(defun search-selection-backward (sel-begin sel-end)
  "Search for the selected text backward."
  (interactive "r") ; Needs a selection.
  (deactivate-mark)
  (search-text-between sel-begin sel-end nil))

(with-eval-after-load 'evil
  (bind-key "*" #'search-selection-forward evil-visual-state-map)
  (bind-key "#" #'search-selection-backward evil-visual-state-map))

(provide 'conf/visual/search-selected)
