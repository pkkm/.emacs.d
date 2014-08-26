;;; Display ^L (page breaks) as horizontal lines.

(use-package page-break-lines
  :ensure page-break-lines
  :diminish page-break-lines-mode
  :commands (page-break-lines-mode global-page-break-lines-mode)
  :init
  (global-page-break-lines-mode))

(provide 'conf/view/page-break-lines)
