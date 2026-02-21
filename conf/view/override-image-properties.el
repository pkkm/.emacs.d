;;; A mechanism for overriding the properties of all images in the buffer. -*- lexical-binding: t -*-

;; TODO: Emacs feature request to introduce a variable for this. E.g. `inline-image-max-size', a cons cell of two numbers for max width and height, with a number <= 1 being a percentage of window size and larger number being pixels. For example, `(1 . 0.5)' would let inline images use the full width of the window but only half the height. Also, does `image-auto-resize' only work when opening image files, or does it affect inline images too?

(defvar-local my-image-override-properties nil
  "Plist of properties to override on all images in buffer.
Example: '(:width my-remove-entry :max-width 640 :max-height 480)")

(require 'cl-lib) ; Used: cl-loop.
(require 'map) ; Used: map-delete.
(defun my-plist-merge (first &rest rest)
  "Merge plists, with ones to the right overriding the ones to the left.
The special value `my-remove-entry' will cause the key to be removed from the result."
  (let ((result (copy-sequence first)))
    (dolist (new-plist rest)
      (cl-loop for (key value) on new-plist by 'cddr
               do (setq result
                        (if (eq value 'my-remove-entry)
                            (map-delete result key)
                          (plist-put result key value)))))
    result))

(defun my-create-image-advice-apply-override (orig-fun file-or-data &optional type data-p &rest props)
  (if my-image-override-properties
      (progn
        (when (and (not (eq type 'imagemagick))
                   (not (image-transforms-p)))
          (error "my-image-override properties needs image transforms support"))
        (let ((merged-props (my-plist-merge props my-image-override-properties)))
          (apply orig-fun file-or-data type data-p merged-props)))
    (apply orig-fun file-or-data type data-p props)))
(advice-add 'create-image :around #'my-create-image-advice-apply-override)

(provide 'conf/view/override-image-properties)
