;;; Scrolling. -*- lexical-binding: t -*-

;; Keep 3 lines visible around the point.
(setq-default scroll-margin 3)

;; When the point would move into the margin, scroll one line at a time instead of by percentage.
(setq scroll-conservatively 101)

(provide 'conf/view/scrolling)
