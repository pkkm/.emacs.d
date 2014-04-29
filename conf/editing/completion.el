;;; Completion.

(require 'conf/packages)
(package-ensure-installed 'auto-complete)

(global-auto-complete-mode 1)

;; Make TAB indent on the first press, and activate completion on the second.
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; `completion-at-point' -- when TAB is pressed and the current line is already properly indented.
;;   * If there's a complete snippet name before point, expand it using YASnippet (if YASnippet minor mode is active).
;;   * Otherwise, launch `auto-complete'.
;; Note that this won't be used when `auto-complete' is displayed (the TAB binding from `ac-completing-map' will be used instead).
(require 'conf/editing/snippets)
(defun yas-expand-if-active ()
  "Call `yas-expand' if YASnippet minor mode is active."
  (when yas-minor-mode
    (yas-expand)))
(setq yas-fallback-behavior 'return-nil) ; To make it work with `completion-at-point'.
(setq-default completion-at-point-functions (cons #'yas-expand-if-active completion-at-point-functions))
(defun set-auto-complete-as-completion-at-point ()
  (setq completion-at-point-functions '(yas-expand-if-active auto-complete)))
(add-hook 'auto-complete-mode-hook #'set-auto-complete-as-completion-at-point)

;; Don't disable auto-complete on specific faces.
(setq ac-disable-faces nil)


;;; Bindings.

(setq ac-completing-map (make-sparse-keymap)) ; There is also `ac-menu-map', for when the menu is active.

;; Selecting a candidate and completing.
(define-key ac-completing-map (kbd "TAB") #'ac-expand) ; Expand; cycle candidates when pressed repeatedly.
(define-key ac-completing-map (kbd "<backtab>") #'ac-previous)
(define-key ac-completing-map (kbd "RET") #'ac-complete) ; Choose the current candidate and perform its associated action.

;; Help.
(define-key ac-completing-map (kbd "M-p") #'ac-quick-help-scroll-up)
(define-key ac-completing-map (kbd "M-n") #'ac-quick-help-scroll-down)
(define-key ac-completing-map (kbd "<f1>") #'ac-persist-help) ; Open help in a buffer.


;;; Sources.

(require 'conf/editing/snippets)
(require 'auto-complete-config) ; Used: ac-source-yasnippet.
(setq-default ac-sources
              '(;;ac-source-semantic
                ac-source-words-in-buffer
                ac-source-yasnippet
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers
                ac-source-words-in-all-buffer
                ac-source-filename))

(defvar my-major-mode-ac-sources '()
  "`auto-complete' sources to use in various major-modes.
Format: '((major-mode . (ac-source ...)) ...)")

(require 'cl-lib)
(defun my-set-ac-sources ()
  "Add `auto-complete' sources for the given major mode, based on `my-ac-major-mode-sources'."
  (let ((mode-sources (cdr (assoc major-mode my-major-mode-ac-sources))))
    (setq ac-sources (cl-remove-duplicates (append mode-sources ac-sources) :from-end t)))) ; If a source occurs more than once, retain the earliest occurrence.
(add-hook 'after-change-major-mode-hook #'my-set-ac-sources)


;;; Misc.

;; Automatic activation.
(setq ac-auto-start 1) ; Start completion after 1 letter.
(setq ac-delay 0.05) ; Delay before starting completion.
(setq ac-auto-show-menu t) ; Show menu immediately (skip showing a candidate inline).

;; If `auto-complete' is too slow:
;; * Lower `ac-candidate-limit'
;; * Change `ac-auto-start' to a higher number, so that `auto-complete' doesn't have to consider as many candidates

;; Help.
(setq ac-quick-help-delay 1)

;; Savefile.
(setq ac-comphist-file
      (expand-file-name "auto-complete-history" my-savefile-dir))


(provide 'conf/editing/completion)
