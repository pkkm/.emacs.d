;;; Modeline format. -*- lexical-binding: t -*-

(defun my-modeline-shorten-path (path max-length)
  "Shorten PATH to up to MAX-LENGTH characters."
  (let ((reversed-path-list (reverse (file-name-split (abbreviate-file-name path))))
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

(defun my-concat-nonempty (separator &rest parts)
  "Return string with nonempty (not nil or \"\") elements of PARTS separated with SEPARATOR."
  (declare (indent defun))
  (string-join (delq nil (delete "" parts)) separator))

;; Don't display the version control system name (e.g. "Git") in the modeline.
(setq vc-display-status 'no-backend)

(defface ml-shadow `((t (:inherit shadow)))
  "Face for deemphasized parts of the modeline."
  :group 'modeline)

(setq-default
 mode-line-format
 (list
  " "

  '(:eval
    (my-concat-nonempty " "
      (let ((shortened-dir
             (when buffer-file-name ; If the buffer is visiting a file...
               (propertize (my-modeline-shorten-path
                            (abbreviate-file-name (file-name-directory buffer-file-name))
                            (/ (window-total-width) 3))
                           'face 'ml-shadow))))
        (concat shortened-dir "%b"))
      (when buffer-read-only (propertize "RO" 'face '(:weight bold)))
      (when (buffer-modified-p) (propertize "+" 'face 'warning))
      (when (buffer-narrowed-p) (propertize "Narrow" 'face '(:underline t)))))

  'mode-line-format-right-align

  (propertize "%[" 'face 'ml-shadow) ; Recursive edit braces.
  '(:eval
    (my-concat-nonempty " "
      (format-mode-line '("" mode-name mode-line-process minor-mode-alist))
      (format-mode-line mode-line-misc-info))) ; Used for example by `display-time'.
  (propertize "%]" 'face 'ml-shadow)

  " ┃" ; Alternative: (propertize " │ " 'face 'ml-shadow) (from <https://en.wikipedia.org/wiki/Box-drawing_character>).

  '(project-mode-line project-mode-line-format)
  'vc-mode
  " "
  '(:eval
    (my-concat-nonempty " "
      ;; Coding system (hide if utf-8-unix or equivalent).
      (unless (memq buffer-file-coding-system
                    '(utf-8-unix prefer-utf-8-unix undecided-unix))
        (symbol-name buffer-file-coding-system))

      (string-replace "%" "%%" (format-mode-line '(-3 "%p"))) ; Position, limited to 3 characters (e.g. "56%" or "Bot").

      "%2l:%c")) ; Line and column.

  " "))

(provide 'conf/view/modeline/format)
