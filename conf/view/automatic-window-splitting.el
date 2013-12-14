;;; How to split windows automatically (for example, for help).

(defvar my-split-window/form-factor 2
  "If width/height of the window is at least as large as this number, split right, otherwise below.")

(defvar my-split-window/min-width 80
  "Don't split right if the resulting window would be narrower than this.")
(defvar my-split-window/min-height 24
  "Don't split below if the resulting window would be shorter than this.")

(defun my-split-window (&optional windowa)
  "If width/height of WINDOW is at least as large than `my-split-window/form-factor', split it horizontally, otherwise vertically.
Also observe the lower limits on window size: `my-split-window/min-width', `my-split-window/min-height'."
  (interactive)
  (let* ((window (or windowa (selected-window)))
         (width (window-width window))
         (height (window-height window))
         (can-split-right (>= (/ width 2) my-split-window/min-width))
         (can-split-below (>= (/ height 2) my-split-window/min-height))
         (form-factor-split-right (>= width (* my-split-window/form-factor height))))
    (cond
     ((and form-factor-split-right
           can-split-right)
      (with-selected-window window
        (split-window-right)))
     ((and (not form-factor-split-right)
           can-split-below)
      (with-selected-window window
        (split-window-below)))
     (can-split-right
      (with-selected-window window
        (split-window-right)))
     (can-split-below
      (with-selected-window window
        (split-window-below)))
     (t
      nil))))

(setq split-window-preferred-function #'my-split-window)

(provide 'conf/view/automatic-window-splitting)
