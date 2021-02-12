;;; Modeline format. -*- lexical-binding: t -*-


;;; Helper functions.

(defun ml-shorten-path (path max-length)
  "Shorten PATH to up to MAX-LENGTH characters."
  (let ((reversed-path-list (reverse (split-string (abbreviate-file-name path) "/")))
        (result "")
        (prefix-when-shortened "…/"))
    (when reversed-path-list
      (when (equal "" (car reversed-path-list)) ; Ignore trailing slash.
        (setq reversed-path-list (cdr reversed-path-list)))
      (let ((new-path))
        (while (and reversed-path-list
                    (setq new-path (concat (car reversed-path-list) "/" result)) ; The return value doesn't matter (it's always non-nil).
                    (<= (+ (length new-path) (length prefix-when-shortened))
                        max-length))
          (setq result new-path)
          (setq reversed-path-list (cdr reversed-path-list))))
      (when reversed-path-list
        (setq result (concat prefix-when-shortened result))))
    result))

(defun ml-concat-nonempty (separator &rest parts)
  "Return string with nonempty (not nil or \"\") elements of PARTS separated with SEPARATOR."
  (declare (indent defun))
  (mapconcat #'identity (delq nil (delete "" parts)) separator))


;;; Alignment.
;; Fill functions are from <https://github.com/milkypostman/powerline>.

(defvar ml-text-scale-factor 1.0
  "Scale of mode-line font size to default font size, as a float.
This is needed to make sure that text is properly aligned.")

(defun ml-fill-to-center (reserve face)
  "Return empty space to the center, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin))
                                             ,reserve
                                             (.5 . left-margin))))
              'face face))

(defun ml-fill-to-right (reserve face)
  "Return empty space, leaving RESERVE space on the right."
  (when ml-text-scale-factor
    (setq reserve (* ml-text-scale-factor reserve)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 2))) ; Powerline uses 3 here, but my scrollbars are narrower.
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve)))
              'face face))

(defun ml-render-2-part (left right &optional fill-face)
  (concat left
          (ml-fill-to-right (string-width (format-mode-line right)) fill-face)
          right))

(defun ml-render-3-part (left center right &optional fill-face)
  (concat left
          (ml-fill-to-center (/ (string-width (format-mode-line center)) 2.0) fill-face)
          center
          (ml-fill-to-right (string-width (format-mode-line right)) fill-face)
          right))


;;; The actual format.

;; "Helper" face for less important parts of the modeline.
(defface ml-shadow `((t ())) ; Foreground of `mode-line-inactive'.
  "Face for de-emphasized parts of the modeline."
  :group 'modeline)
(defun set-mode-line-helper-faces ()
  "Calculate the modeline \"helper\" faces that depend on colors in other faces."
  (set-face-foreground 'ml-shadow (face-foreground 'mode-line-inactive)))
(add-hook 'after-load-theme-hook #'set-mode-line-helper-faces)
(set-mode-line-helper-faces)

(defun ml-format ()
  (let* ((shortened-dir
          (when buffer-file-name ; If the buffer is visiting a file...
            (propertize (ml-shorten-path (abbreviate-file-name (file-name-directory buffer-file-name))
                                         (/ (window-total-width) 3))
                        'face 'ml-shadow)))

         (ml-coding ; Coding system (hide if utf-8-unix or equivalent).
          (unless (memq buffer-file-coding-system
                        '(utf-8-unix prefer-utf-8-unix undecided-unix))
            (symbol-name buffer-file-coding-system))))

    (ml-render-2-part

     ;; Left.
     (concat
      " "
      (ml-concat-nonempty " "
        (concat (or shortened-dir "") "%b")
        (when buffer-read-only (propertize "RO" 'face '(:weight bold)))
        (when (buffer-modified-p) (propertize "+" 'face 'warning))
        (when (buffer-narrowed-p) (propertize "Narrow" 'face '(:underline t)))))

     ;; Right.
     (concat
      (propertize "%[" 'face 'ml-shadow) ; Recursive edit braces.
      (ml-concat-nonempty " "
        (format-mode-line '("" mode-name mode-line-process minor-mode-alist))
        (format-mode-line global-mode-string)) ; Used for example by `display-time'.
      (propertize "%]" 'face 'ml-shadow)
      " ┃ " ; Alternative: (propertize " │ " 'face 'ml-shadow) (from <https://en.wikipedia.org/wiki/Box-drawing_character>).
      (ml-concat-nonempty " "
        ml-coding
        "%p" ; Position (e.g. "56%" or "All").
        "%l:%c") ; Line and column.
      " "))))

(setq-default mode-line-format '((:eval (ml-format))))

(provide 'conf/view/modeline/format)
