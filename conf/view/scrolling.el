;;; Scrolling.

;; Keep 3 lines visible around the point.
(setq-default scroll-margin 3)

;; Scroll by 5% of the window.
(setq-default scroll-up-aggressively 0.05
              scroll-down-aggressively 0.05)

(provide 'conf/view/scrolling)
