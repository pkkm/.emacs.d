;;; Auto-completion.

(require 'conf/packages)
(package-ensure-installed 'auto-complete)

;; Savefile.
(setq ac-comphist-file
      (expand-file-name "auto-complete-history" my-savefile-dir))

(global-auto-complete-mode 1)

(setq ac-auto-start 2) ; Start completion after how many letters? (nil to disable.)
(setq ac-delay 0.1) ; ???.
(setq ac-disable-inline nil)
(setq ac-auto-show-menu 0.8) ; Delay before showing menu.

(setq ac-quick-help-delay 1.5)

;; TODO
(setq-default ac-sources
              '(ac-source-dictionary
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-words-in-all-buffer))
;; ac-trigger-key

;;; Bindings.

;; TODO make this work.
(require 'conf/evil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

(setq ac-completing-map (make-sparse-keymap))

(define-key ac-completing-map (kbd "TAB") #'ac-expand)
(define-key ac-completing-map (kbd "C-n") #'ac-next)
(define-key ac-completing-map (kbd "C-p") #'ac-previous)

(define-key ac-completing-map (kbd "<f1>") #'ac-help)
(define-key ac-completing-map (kbd "M-<f1>") #'ac-persist-help)
(define-key ac-completing-map (kbd "C-<up>") #'ac-quick-help-scroll-up)
(define-key ac-completing-map (kbd "C-<down>") #'ac-quick-help-scroll-down)


(provide 'conf/editing/auto-completion)
