;;; Remap basic motions to [htns] (for a Dvorak keyboard).
;;; Also remap some related keys.

(with-eval-after-load 'evil
  ;; Left, down, up, right: [htns] (previously: [hjkl]).
  (bind-key "h" #'evil-backward-char evil-motion-state-map)
  (bind-key "t" #'evil-next-line evil-motion-state-map)
  (bind-key "n" #'evil-previous-line evil-motion-state-map)
  (bind-key "s" #'evil-forward-char evil-motion-state-map) (bind-key "s" nil evil-normal-state-map)

  ;; Down/up displayed line (as opposed to physical line): gt/gn (previously: gj/gk).
  (bind-key "gt" #'evil-next-visual-line evil-motion-state-map) (bind-key "gj" nil evil-motion-state-map)
  (bind-key "gn" #'evil-previous-visual-line evil-motion-state-map) (bind-key "gk" nil evil-motion-state-map)

  ;; First non-blank/last character of the line: H/S (previously: H/L -- first/last visible line in the window).
  (bind-key "H" #'evil-first-non-blank evil-motion-state-map)
  (bind-key "S" #'evil-end-of-line evil-motion-state-map) (bind-key "S" nil evil-normal-state-map)

  ;; Join lines with/without changing whitespace: T/gT (previously: J/gJ).
  (bind-key "T" #'evil-join evil-normal-state-map) (bind-key "T" nil evil-motion-state-map)
  (bind-key "gT" #'evil-join-whitespace evil-normal-state-map)

  ;; Jump before a given character forward/backward: j/J (previously: t/T).
  (bind-key "j" #'evil-find-char-to evil-motion-state-map)
  (bind-key "J" #'evil-find-char-to-backward evil-motion-state-map) (bind-key "J" nil evil-normal-state-map)

  ;; Substitute (kill): k (previously: s).
  (bind-key "k" #'evil-substitute evil-normal-state-map) (bind-key "k" nil evil-motion-state-map)
  ;; K -- look up the keyword at point (like `man').

  ;; Next/previous search result: l/L (previously: n/N).
  (bind-key "l" #'evil-search-next evil-motion-state-map)
  (bind-key "L" #'evil-search-previous evil-motion-state-map))

(provide 'conf/useless-without-evil/vim-dvorak)
