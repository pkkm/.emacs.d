;;; Display ^L (page breaks) as horizontal lines.

(use-package page-break-lines
  :ensure t
  :defer t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

(provide 'conf/view/page-break-lines)
