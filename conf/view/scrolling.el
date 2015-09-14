;;; Scrolling.

;; Keep 3 lines visible around the point.
(setq-default scroll-margin 3)

;; Scroll by 10% of the window.
(setq-default scroll-up-aggressively 0.1
              scroll-down-aggressively 0.1)

(provide 'conf/view/scrolling)
