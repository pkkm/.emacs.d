;;; Color identifiers (variables, etc.) based on their name.

(require 'conf/packages)
(package-ensure-installed 'color-identifiers-mode)
(global-color-identifiers-mode)

(provide 'conf/view/color-identifiers)
