;;; UI elements (toolbar, menubar, etc). -*- lexical-binding: t -*-

(tool-bar-mode -1)
(menu-bar-mode -1)

(scroll-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(provide 'conf/view/ui-elements)
