;;; `require' my customizations based on some conditions.

(require 'conf/init/check-version)
(require 'conf/init/no-startup-message)
(require 'conf/init/initial-scratch-message)

(require 'conf/packages)
(require 'conf/evil)

(require 'conf/view/trailing-whitespace)
(require 'conf/view/automatic-window-splitting)
(require 'conf/view/windows)
(require 'conf/view/hl-current-line)
(require 'conf/view/buffer-map)
(require 'conf/view/color-theme)
(require 'conf/view/wrapping)
(require 'conf/view/scrolling)
(require 'conf/view/ui-elements) ;;!!!
(require 'conf/view/unique-buffer-names)
(require 'conf/view/page-break-lines)

(when window-system
  (require 'conf/view/gui/title)
  (require 'conf/view/gui/font) ;;!!!
  (require 'conf/view/gui/cursor)
  (require 'conf/view/gui/fringe)
  (require 'conf/view/gui/start-maximized)
  (require 'conf/view/gui/focus-follows-mouse))

(require 'conf/minibuffer/y-or-n-p)
(require 'conf/minibuffer/esc-quits-prompts)
(require 'conf/minibuffer/ido)
(require 'conf/minibuffer/M-x)
(require 'conf/minibuffer/default-choice)

(require 'conf/visual/search-selected)
(require 'conf/visual/expand-region)
(require 'conf/visual/undo-in-region)
(require 'conf/visual/text-object-whole-buffer)

(require 'conf/insert-state/insert-verbatim)
(require 'conf/insert-state/esc-alternatives)

(require 'conf/motions/little-word)
(require 'conf/motions/vim-dvorak)
(require 'conf/motions/g-TAB-as-TAB)
(require 'conf/motions/ace-jump)

(require 'conf/editing/indentation)
(require 'conf/editing/smartparens)
(require 'conf/operators/hungry-delete)

(require 'conf/modeline/faces)
(require 'conf/modeline/format)
(require 'conf/modeline/cleaner-minor-modes)

(require 'conf/opening-saving/backups)
(require 'conf/opening-saving/move-delete)
(require 'conf/opening-saving/ag)
(require 'conf/opening-saving/auto-revert)
(require 'conf/opening-saving/editorconfig)
(require 'conf/opening-saving/save-point-position)
(require 'conf/opening-saving/keys)
(require 'conf/opening-saving/ido)
(require 'conf/opening-saving/coding)
(require 'conf/opening-saving/vim-modelines)
(require 'conf/opening-saving/recent-files)

(require 'conf/operators/insert-newline)
(require 'conf/operators/break-line)
(require 'conf/operators/clipboard)
(require 'conf/operators/prefix-arguments)
(require 'conf/operators/inc-dec-number)
(require 'conf/operators/yank-to-eol)

(require 'conf/configuring/unbound)
(require 'conf/configuring/flatten-conf)
(require 'conf/configuring/command-log)

(require 'conf/other/enable-confusing-commands)
(require 'conf/other/guide-key)
(require 'conf/other/convenient-prefix-keys)
;;(require 'conf/other/clipboard)
(require 'conf/other/count-lines-words-chars)
(require 'conf/other/isend)

(require 'conf/mode-specific/fundamental)
(require 'conf/mode-specific/c-and-c++)
(require 'conf/mode-specific/compilation)
(require 'conf/mode-specific/org)
(require 'conf/mode-specific/vimrc)
(require 'conf/mode-specific/elisp-and-interaction)
(require 'conf/mode-specific/lisps)
(require 'conf/mode-specific/scala)
(require 'conf/mode-specific/sgml)
(require 'conf/mode-specific/pascal)
(require 'conf/mode-specific/asciidoc)
(require 'conf/mode-specific/text)
(require 'conf/mode-specific/term)
(require 'conf/mode-specific/bat-cmd)

(provide 'conf/-)
