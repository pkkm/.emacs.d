;;; Alternatives to ESC for exiting the insert and replace states.

(require 'conf/evil)

;; Key-chord -- allow defining "chords" of 2 keys.
;; The reverse chord (ex. "jk", "kj") will automatically be mapped.
(require 'conf/packages)
(package-ensure-installed 'key-chord)
(require 'conf/utils/ignore-messages)
(ignore-specific-messages '("Key Chord mode on")
 (key-chord-mode 1))
(setq key-chord-one-key-delay 0.3) ; Delay when the chord is a key repeated 2 times.
(setq key-chord-two-keys-delay 0.3) ; Delay when the chord consists of 2 different keys.

;; "hh".
(key-chord-define evil-insert-state-map "hh" #'evil-normal-state)
(key-chord-define evil-replace-state-map "hh" #'evil-normal-state)

;; C-SPC and S-SPC.
(mapcar
 (lambda (key)
   (define-key evil-insert-state-map key #'evil-normal-state)
   (define-key evil-replace-state-map key #'evil-normal-state))
 (list (kbd "C-SPC")
       (kbd "S-SPC")))

(provide 'conf/insert-state/esc-alternatives)
