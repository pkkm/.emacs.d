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

;; Documentation popups.
(use-package company-quickhelp
  ;; Use the git version because there's a company-tng compatibility fix that hasn't been released yet: <https://github.com/company-mode/company-quickhelp/commit/9505fb09d064581da142d75c139d48b5cf695bd5>.
  :preface
  (package-ensure-version 'company-quickhelp "20231026.1714")

  ;; Make colors match the theme. Done here because these variables don't always take effect immediately.
  (defun my-set-company-quickhelp-colors ()
    (setq company-quickhelp-color-foreground (face-foreground 'default))
    (setq company-quickhelp-color-background (face-background 'default)))
  (add-hook 'after-load-theme-hook #'my-set-company-quickhelp-colors)
  (my-set-company-quickhelp-colors)

  :init
  (with-eval-after-load 'company
    (company-quickhelp-mode))
  :config
  (setq company-quickhelp-delay 1))

(provide 'conf/editing/completion)
