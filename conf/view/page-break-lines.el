;;; Display ^L (page breaks) as horizontal lines. -*- lexical-binding: t -*-

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

(provide 'conf/view/page-break-lines)
