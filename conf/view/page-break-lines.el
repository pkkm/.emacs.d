;;; Display ^L (page breaks) as horizontal lines.

(require 'conf/packages)
(package-ensure-installed 'page-break-lines)

(global-page-break-lines-mode 1)

(require 'conf/modeline/cleaner-minor-modes)
(diminish 'page-break-lines-mode "")

(provide 'conf/view/page-break-lines)
