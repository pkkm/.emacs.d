;;; Lacarte -- execute menu commands using the keyboard.

(use-package lacarte
  :ensure lacarte
  :bind ("C-x M" . lacarte-execute-menu-command))

(provide 'conf/minibuffer/lacarte)
