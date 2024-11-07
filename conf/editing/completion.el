;;; Completion. -*- lexical-binding: t -*-
;; For information about the current backend and completions, use M-x company-diag.

;; Make TAB indent on the first press, and activate completion on the second.
(setq tab-always-indent 'complete)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode)
  :config

  ;; Automatic activation.
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 2) ; Start completion after 2 letters.

  ;; Use TAB for both selecting and changing the completion.
  (add-to-list 'company-frontends #'company-tng-frontend)
  (setq company-require-match 'never)

  ;; Don't downcase candidates. (See also `company-dabbrev-ignore-case'.)
  (setq company-dabbrev-downcase nil)

  ;; Bindings.
  (bind-key "<tab>" nil company-active-map)
  (bind-key "TAB" #'company-select-next company-active-map)
  (bind-key "<backtab>" #'company-select-previous company-active-map)
  (bind-key "<return>" nil company-active-map)
  (bind-key "RET" nil company-active-map)
  (bind-key "C-w" nil company-active-map)

  ;; Don't show single candidates inline (it's buggy as of 2018-11).
  (setq company-frontends
        (-replace 'company-pseudo-tooltip-unless-just-one-frontend
                  'company-pseudo-tooltip-frontend
                  (delq 'company-preview-if-just-one-frontend company-frontends)))

  ;; Use Company for `completion-at-point'.
  (require 'conf/utils/functions) ; Used: define-interactive-wrapper.
  (require 'conf/utils/lists) ; Used: add-to-list-after.
  (define-interactive-wrapper company-complete-wrapper (&rest args) company-complete
    (let ((completion-at-point-functions
           (-remove-item #'company-wrapper-if-active completion-at-point-functions)))
      (diw-apply-original-fun args)))
  (defun company-wrapper-if-active ()
    (when company-mode #'company-complete-wrapper))
  (defun add-company-to-completion-at-point ()
    (add-to-list-after 'completion-at-point-functions
                       #'company-wrapper-if-active
                       #'yas-expand-if-active))
  (add-hook 'company-mode-hook #'add-company-to-completion-at-point))

;; Documentation popups.
(use-package company-quickhelp
  :ensure t
  :init
  (with-eval-after-load 'company
    (company-quickhelp-mode))
  :config
  (setq company-quickhelp-delay 1))

(provide 'conf/editing/completion)
