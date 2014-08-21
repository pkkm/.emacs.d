;;; Display ^L (page breaks) as horizontal lines.

(require 'conf/packages)

(use-package page-break-lines
  :ensure page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(provide 'conf/view/page-break-lines)
