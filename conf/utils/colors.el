;;; Functions for manipuating colors.

(require 'color) ; Used: color-rgb-to-hex, color-name-to-rgb, color-defined-p.
(use-package dash :ensure dash) ; Used: -partition, -all?.
(require 'cl-lib)

(defun color-mix (&rest colors-weights-list)
  "Mix colors in the given proportions.
Example: (color-mix \"#ffffff\" 0.6 \"#ff0000\" 0.4) => \"#ff9999\".
If the sum of weights (proportions) is larger than 1.0, the result will be brighter.
If one of the colors is unspecified (cannot be displayed), return 'unspecified."
  (let* ((color-weight-pairs (-partition 2 colors-weights-list)) ; '((color weight)...)
         (all-colors-defined-p (-all? (lambda (color-weight-pair)
                                        (color-defined-p (car color-weight-pair)))
                                      color-weight-pairs)))
    (if all-colors-defined-p
        (apply 'color-rgb-to-hex
         (cl-reduce ;; Sum the list of (red green blue) triples.
          (lambda (rgb-1 rgb-2)
            (cl-mapcar '+ rgb-1 rgb-2))
          (mapcar ;; Change pairs of (color-name weight) to triples of (red green blue).
           (lambda (color-weight-pair)
             (let ((color-rgb-components (color-name-to-rgb (car color-weight-pair)))
                   (weight (cadr color-weight-pair)))
               (mapcar (lambda (color-component)
                         (* color-component weight))
                       color-rgb-components)))
           color-weight-pairs)))
      'unspecified)))

(provide 'conf/utils/colors)
