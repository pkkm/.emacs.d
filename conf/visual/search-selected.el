;;; Search for the selected text.

(require 'conf/evil)

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

(define-key evil-visual-state-map (kbd "*") 'search-selection-forward)
(define-key evil-visual-state-map (kbd "#") 'search-selection-backward)

(provide 'conf/visual/search-selected)
