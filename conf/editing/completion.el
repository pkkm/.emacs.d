;;; Completion.

;; Make TAB indent on the first press, and activate completion on the second.
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t) ; E.g. ~/sew -> ~/src/emacs/work.

(use-package auto-complete
  :ensure auto-complete
  :diminish auto-complete-mode
  :defer t
  :init

  (defvar my-major-mode-ac-sources '()
    "Additional `auto-complete' sources to use in various major-modes.
Format: '((major-mode . (ac-source ...)) ...)")
  ;; This has to be here because it's used by other parts of my config.

  ;; Enable auto-complete everywhere (apart from the minibuffer).
  (defun my-enable-auto-complete ()
    "Enable auto-complete if we're not in the minibuffer."
    (unless (minibufferp)
      (auto-complete-mode 1)))
  (define-globalized-minor-mode
    my-auto-complete-everywhere-mode
    auto-complete-mode
    my-enable-auto-complete)
  (my-auto-complete-everywhere-mode 1)

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
    (push newelt (cdr (nthcdr index (symbol-value list-name)))))
  (defun add-ac-to-completion-at-point ()
    (unless (memq #'auto-complete-if-active completion-at-point-functions)
      (let ((position (cl-position #'yas-expand-if-active completion-at-point-functions)))
        (if position
            (insert-after-index 'completion-at-point-functions position #'auto-complete-if-active)
          (push #'auto-complete-if-active completion-at-point-functions)))))
  (add-hook 'auto-complete-mode-hook #'add-ac-to-completion-at-point)


  ;;; Bindings.

  ;; Delete default bindings.
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap ac-completing-map) ; There is also `ac-menu-map', for when the menu is active.

  ;; Cycle candidates with arrow keys.
  (bind-key "<down>" #'ac-next ac-completing-map)
  (bind-key "<up>" #'ac-previous ac-completing-map)

  ;; Completing.
  (bind-key "TAB" #'ac-expand ac-completing-map) ; Expand; cycle candidates when pressed repeatedly.
  (bind-key "<backtab>" #'ac-previous ac-completing-map)
  (bind-key "RET" #'ac-complete ac-completing-map) ; Choose the current candidate and perform its associated action.

  ;; Help.
  (bind-key "M-p" #'ac-quick-help-scroll-up ac-completing-map)
  (bind-key "M-n" #'ac-quick-help-scroll-down ac-completing-map)
  (bind-key "<f1>" #'ac-persist-help ac-completing-map) ; Open help in a buffer.


  ;;; Sources.

  (require 'auto-complete-config) ; Used: ac-source-yasnippet.
  (setq-default ac-sources '(ac-source-semantic
                             ac-source-words-in-buffer
                             ac-source-yasnippet
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-words-in-all-buffer
                             ac-source-filename))

  ;; Automatically add sources from `my-major-mode-ac-sources'.
  (use-package dash :ensure dash :commands (--> -map))
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
