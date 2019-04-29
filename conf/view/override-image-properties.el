;;; A mechanism for overriding the properties of all images in the buffer. -*- lexical-binding: t -*-

(defvar my-image-override-properties nil
  "Plist of properties to override on all images in buffer. Requires imagemagick support.
Example: '(:width my-remove-entry :max-width 640 :max-height 480)")
(make-variable-buffer-local 'my-image-override-properties)

(require 'cl-lib) ; Used: cl-loop.
(require 'org-macs) ; Used: org-plist-delete.
(defun my-plist-merge (first &rest rest)
  "Merge plists, with ones to the right overriding the ones to the left.
The special value `my-remove-entry' will cause the key to be removed from the result."
  (let ((result (copy-sequence first)))
    (dolist (new-plist rest)
      (cl-loop for (key value) on new-plist by 'cddr
               do (setq result
                        (if (eq value 'my-remove-entry)
                            (org-plist-delete result key)
                          (plist-put result key value)))))
    result))

(defadvice create-image (around my-apply-override-properties activate)
  (when my-image-override-properties
    (unless (fboundp 'imagemagick-types)
      (error "my-image-override-properties needs imagemagick support"))
    ;; Arguments of `create-image': (file-or-data &optional type data-p &rest props).
    (ad-set-arg 1 'imagemagick)
    (ad-set-args 3 (my-plist-merge (ad-get-args 3) my-image-override-properties)))
  ;; Useful for debugging:
  ;; (message "my-apply-override-properties: create-image %s" (ad-get-args 0))
  ad-do-it)

(provide 'conf/view/override-image-properties)
