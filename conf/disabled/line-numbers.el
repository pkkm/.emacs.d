;;; Line numbers.

(global-linum-mode 1)

(defun linum-format-toggle ()
  "Toggle line number format between absolute and relative."
  (interactive)
  (if (eq linum-format #'linum-absolute)
      (setq linum-format #'linum-relative) ; Warning: this is an implementation detail of linum-relative.
    (setq linum-format #'linum-absolute)))

;; Change to absolute format when entering insert state, restore previous format when exiting it.
(require 'conf/evil)
(add-hook 'evil-insert-state-entry-hook #'linum-absolute-and-save-format)
(add-hook 'evil-insert-state-exit-hook #'linum-restore-saved-format)
(defvar linum-saved-format nil)
(defun linum-absolute-and-save-format ()
  (setq linum-saved-format linum-format)
  (setq linum-format #'linum-absolute))
(defun linum-restore-saved-format ()
  (setq linum-format linum-saved-format))

;; My absolute line numbers: right-aligned, with space on the left.
(defun linum-absolute (line-number)
  (propertize (format linum-absolute-format line-number)
              'face 'linum))
(defvar linum-absolute-format " %2d " ; Before recalculating (below).
  "The format string for absolute line numbers.")
(defun linum-update-absolute-format ()
  (let* ((max-linum-width (length (number-to-string (count-lines (point-min) (point-max))))) ;; TODO maybe replace (length (number-to-string ...)) with (ceiling (log ... 10))
         (format (concat " %" (number-to-string max-linum-width) "d ")))
    (setq linum-absolute-format format)))
(add-hook 'linum-before-numbering-hook #'linum-update-absolute-format)

;; Relative line numbers. (Space on the left, aligning to the right -- built in.)
(require 'conf/packages)
(package-ensure-installed 'linum-relative)
(require 'linum-relative) ; Load and activate relative line numbers.
(setq linum-relative-current-symbol "0")
(face-spec-set 'linum-relative-current-face `((t :inherit 'linum)) 'face-defface-spec)
(defadvice linum-relative (after linum-relative-append-space activate)
  "Append a space to the linum-relative line number format."
  (setq ad-return-value
        (concat ad-return-value
                (propertize " " 'face 'linum))))

;; Use the default font for line numbers, even if the buffer uses a variable-pitch font.
(when window-system
  (defun linum-use-default-font ()
    "Use the default font, not the buffer-local font, for line numbers."
    (set-face-font 'linum (face-font 'default)))
  (linum-use-default-font)
  (add-hook 'after-setting-font-hook #'linum-use-default-font))

(provide 'conf/view/line-numbers)
