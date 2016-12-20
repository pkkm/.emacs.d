;;; Functions for manipuating colors. -*- lexical-binding: t -*-

(require 'color) ; Used: color-rgb-to-hex, color-name-to-rgb, color-defined-p.
(require 'cl-lib) ; Used: cl-destructuring-bind.

(defun color-mix (&rest colors-weights-list)
  "Mix colors in the given proportions.
Example: (color-mix \"#ffffff\" 0.6 \"#ff0000\" 0.4) => \"#ff9999\".
If the sum of weights (proportions) is larger than 1.0, the result will be brighter.
If one of the colors is unspecified (cannot be displayed), return 'unspecified."
  (let* ((color-weight-pairs (-partition 2 colors-weights-list)) ; '((color weight) ...)
         (all-colors-defined-p (-all? (lambda (color-weight-pair)
                                        (color-defined-p (car color-weight-pair)))
                                      color-weight-pairs)))
    (if all-colors-defined-p
        (->> color-weight-pairs
          ;; Change pairs of (color-name weight) to triples of (red green blue).
          (mapcar (lambda (color-weight-pair)
                    (cl-destructuring-bind (color-name weight) color-weight-pair
                      (let ((color-rgb (color-name-to-rgb color-name)))
                        (mapcar (lambda (rgb-component) (* rgb-component weight))
                                color-rgb)))))
          ;; Sum the list of (red green blue) triples.
          (-reduce (lambda (rgb-1 rgb-2)
                     (cl-mapcar '+ rgb-1 rgb-2)))
          ;; Convert to hex notation.
          (apply 'color-rgb-to-hex))
      'unspecified)))

(provide 'conf/utils/colors)
