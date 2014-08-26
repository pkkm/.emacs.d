;;; Completion.

(require 'conf/packages)

;; Make TAB indent on the first press, and activate completion on the second.
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t) ; E.g. ~/sew -> ~/src/emacs/work.

(use-package auto-complete
  :ensure auto-complete
  :commands global-auto-complete-mode
  :init
  (global-auto-complete-mode 1)
  :config

  ;; Don't disable auto-complete on specific faces.
  (setq ac-disable-faces nil)

  ;; Automatic activation.
  (setq ac-auto-start 1) ; Start completion after 1 letter.
  (setq ac-delay 0.05)
  (setq ac-auto-show-menu t) ; Show menu immediately (skip showing a candidate inline).

  ;; If `auto-complete' is too slow:
  ;;   * Lower `ac-candidate-limit'
  ;;   * Change `ac-auto-start' to a higher number, so that `auto-complete' doesn't have to consider as many candidates

  ;; Help.
  (setq ac-quick-help-delay 1)

  ;; Savefile.
  (setq ac-comphist-file
        (expand-file-name "auto-complete-history" my-savefile-dir))

  ;; Use `auto-complete' for `completion-at-point'.
  ;; `completion-at-point' is used when TAB is pressed, the current line is already properly indented and:
  ;;   * `tab-always-indent' is set to 'complete
  ;;   * `auto-complete' isn't already being displayed (when it is, the TAB binding in `ac-completing-map' is used instead)
  (require 'cl-lib) ; Used: cl-position.
  (defun auto-complete-if-active ()
    "Call `auto-complete' if `auto-complete-mode' is active."
    (when auto-complete-mode
      (auto-complete)))
  (defun insert-after-index (list-name index newelt)
    (push newelt (cdr (nthcdr index (symbol-value list-name))))
    (symbol-value list-name))
  (defun set-auto-complete-as-completion-at-point ()
    (if (memq #'yas-expand-if-active completion-at-point-functions) ; Integration with conf/editing/snippets.el.
        (insert-after-index 'completion-at-point-functions
                            (cl-position #'yas-expand-if-active completion-at-point-functions)
                            #'auto-complete-if-active)
      (add-to-list 'completion-at-point-functions #'auto-complete-if-active)))
  (add-hook 'auto-complete-mode-hook #'set-auto-complete-as-completion-at-point)


  ;;; Bindings.

  (setq ac-completing-map (make-sparse-keymap)) ; There is also `ac-menu-map', for when the menu is active.

  ;; Cycle candidates with C-n and C-p, or activate `auto-complete' if it's not active.
  (require 'conf/evil)
  (evil-define-key 'insert ac-mode-map (kbd "C-n") #'auto-complete)
  (evil-define-key 'insert ac-mode-map (kbd "C-p") #'auto-complete)
  (define-key ac-completing-map (kbd "C-n") #'ac-next)
  (define-key ac-completing-map (kbd "C-p") #'ac-previous)

  ;; Completing.
  (define-key ac-completing-map (kbd "TAB") #'ac-expand) ; Expand; cycle candidates when pressed repeatedly.
  (define-key ac-completing-map (kbd "<backtab>") #'ac-previous)
  (define-key ac-completing-map (kbd "RET") #'ac-complete) ; Choose the current candidate and perform its associated action.

  ;; Help.
  (define-key ac-completing-map (kbd "M-p") #'ac-quick-help-scroll-up)
  (define-key ac-completing-map (kbd "M-n") #'ac-quick-help-scroll-down)
  (define-key ac-completing-map (kbd "<f1>") #'ac-persist-help) ; Open help in a buffer.


  ;;; Sources.

  (require 'auto-complete-config) ; Used: ac-source-yasnippet.
  (setq-default ac-sources '(;;ac-source-semantic
                             ac-source-words-in-buffer
                             ac-source-yasnippet
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-words-in-all-buffer
                             ac-source-filename))

  (defvar my-major-mode-ac-sources '()
    "Additional `auto-complete' sources to use in various major-modes.
Format: '((major-mode . (ac-source ...)) ...)")

  (use-package dash :ensure dash) ; Used: -->, -map.
  (require 'conf/utils/modes) ; Used: derived-mode-hierarchy.
  (require 'cl-lib) ; Used: cl-remove-duplicates.
  (defun my-set-ac-sources ()
    "Add `auto-complete' sources from `my-ac-major-mode-sources' for the given major mode (and its parent modes)."
    (setq ac-sources
          (--> (derived-mode-hierarchy major-mode)
            (-map (lambda (mode)
                    (cdr (assoc mode my-major-mode-ac-sources)))
                  it)
            (apply #'append it)
            (append it ac-sources)
            (cl-remove-duplicates it :from-end t)))) ; If a source occurs more than once, retain the earliest occurrence.
  (add-hook 'after-change-major-mode-hook #'my-set-ac-sources))


(provide 'conf/editing/completion)
