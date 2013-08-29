;;; WriteGood mode -- highlight bad writing style.

(require 'conf/packages)
(package-ensure-installed 'writegood-mode)

(require 'conf/utils/hooks)
(add-one-shot-hook
 'writegood-mode-hook
 (lambda ()
   ;; Use 'font-lock-warning-face for style warnings.
   (face-spec-set 'writegood-duplicates-face `((t :inherit 'font-lock-warning-face)) 'face-defface-spec)
   (face-spec-set 'writegood-passive-voice-face `((t :inherit 'font-lock-warning-face)) 'face-defface-spec)
   (face-spec-set 'writegood-weasels-face `((t :inherit 'font-lock-warning-face)) 'face-defface-spec)))

(provide 'conf/other/writegood)
