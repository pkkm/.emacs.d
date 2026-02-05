;;; Completion. -*- lexical-binding: t -*-
;; For information about the current backend and completions, use M-x company-diag.

;; Make TAB indent on the first press, and activate completion on the second (when Company is not active).
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
  ;; This function tweaks Company settings to make that way of using it more convenient, see <https://github.com/company-mode/company-mode/blob/master/company-tng.el>.
  (company-tng-mode)

  ;; Don't downcase abbrev candidates. (See also `company-dabbrev-ignore-case'.)
  (setq company-dabbrev-downcase nil)

  ;; Move the binding for "show definition" from C-w to M-.
  (bind-key "C-w" nil company-active-map)
  (bind-key "M-." #'company-show-location company-active-map)

  ;; Activate the completion popup when TAB is pressed on an already indented line.
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))

;; A better frontend that uses child frames and includes documentation popups.
;; It doesn't mess with text properties in the buffer, but also doesn't work in the terminal.
(use-package company-box
  :ensure t
  :diminish company-box-mode
  :init
  (add-hook 'company-mode-hook #'company-box-mode)
  :config
  (setq company-box-doc-delay 0.8))

(provide 'conf/editing/completion)
