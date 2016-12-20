;;; Lacarte -- execute menu commands using the keyboard. -*- lexical-binding: t -*-

(use-package lacarte
  :ensure t
  :bind ("C-x m" . lacarte-execute-menu-command))

(provide 'conf/minibuffer/lacarte)
