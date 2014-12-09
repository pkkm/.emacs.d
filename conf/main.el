;;; `require' my customizations based on some conditions.

;; The order of these `require's must not be significant.
;; (They are treated as a DAG by my `flatten-conf' utility.)

(require 'conf/evil)

(require 'conf/view/trailing-whitespace)
(require 'conf/view/eldoc)
(require 'conf/view/indent-guides)
(require 'conf/view/hl-current-line)
(require 'conf/view/color-theme)
(require 'conf/view/color-identifiers)
(require 'conf/view/wrapping)
(require 'conf/view/scrolling)
(require 'conf/view/ui-elements)
(require 'conf/view/page-break-lines)
(require 'conf/view/initial-messages)
(require 'conf/view/windows/automatic-window-splitting)
(require 'conf/view/windows/window-bindings)
(require 'conf/view/buffers/unique-buffer-names)
(require 'conf/view/buffers/buffer-bindings)
(require 'conf/view/modeline/background)
(require 'conf/view/modeline/format)

(when (display-graphic-p)
  (require 'conf/view/gui/title)
  (require 'conf/view/gui/resize-pixelwise)
  (require 'conf/view/gui/font)
  (require 'conf/view/gui/cursor)
  (require 'conf/view/gui/fringe)
  (require 'conf/view/gui/start-maximized)
  (require 'conf/view/gui/focus-follows-mouse))

(require 'conf/minibuffer/confirmations)
(require 'conf/minibuffer/lacarte)
(require 'conf/minibuffer/ido)
(require 'conf/minibuffer/M-x)
(require 'conf/minibuffer/default-choice)

(require 'conf/region/search-selected)
(require 'conf/region/expand-region)
(require 'conf/region/undo-in-region)

(require 'conf/moving/little-word)
(require 'conf/moving/prev-next-symbol)
(require 'conf/moving/ace-jump)

(require 'conf/editing/indentation)
(require 'conf/editing/emmet)
(require 'conf/editing/kill-word-with-C-w)
(require 'conf/editing/refactoring)
(require 'conf/editing/completion)
(require 'conf/editing/comments)
(require 'conf/editing/smartparens)
(require 'conf/editing/snippets)
(require 'conf/editing/break-line)
(require 'conf/editing/inc-dec-number)
(require 'conf/editing/hungry-delete)

(require 'conf/driving-processes/ag)
(require 'conf/driving-processes/flycheck)
(require 'conf/driving-processes/isend)
(require 'conf/driving-processes/version-control)
(require 'conf/driving-processes/compiling)

(require 'conf/useless-without-evil/insert-newline)
(require 'conf/useless-without-evil/replace-symbol-operator)
(require 'conf/useless-without-evil/convenient-prefix-keys)
(require 'conf/useless-without-evil/prefix-arguments)
(require 'conf/useless-without-evil/yank-fixes)
(require 'conf/useless-without-evil/marks-registers)
(require 'conf/useless-without-evil/vim-dvorak)
(require 'conf/useless-without-evil/g-TAB-as-TAB)
(require 'conf/useless-without-evil/insert-verbatim)
(require 'conf/useless-without-evil/C-SPC-exits-insert-and-visual)
(require 'conf/useless-without-evil/text-object-whole-buffer)

(require 'conf/opening-saving/backups)
(require 'conf/opening-saving/cygwin)
(require 'conf/opening-saving/create-nonexistent-dirs)
(require 'conf/opening-saving/tramp)
(require 'conf/opening-saving/move-delete)
(require 'conf/opening-saving/auto-revert)
(require 'conf/opening-saving/auto-save)
(require 'conf/opening-saving/editorconfig)
(require 'conf/opening-saving/encoding)
(require 'conf/opening-saving/save-point-position)
(require 'conf/opening-saving/bindings)
(require 'conf/opening-saving/ido)
(require 'conf/opening-saving/vim-modelines)
(require 'conf/opening-saving/recent-files)
(require 'conf/opening-saving/diff)

(require 'conf/configuring/unbound)
(require 'conf/configuring/flatten-conf)
(require 'conf/configuring/command-log)

(require 'conf/other/discoverability)
(require 'conf/other/enable-confusing-commands)
(require 'conf/other/portable-keyboard-layout-fix)
(require 'conf/other/publish-realtime-in-browser)
(require 'conf/other/gist)
(require 'conf/other/clipboard)
(require 'conf/other/pastebin)
(require 'conf/other/count-lines-words-chars)

(require 'conf/mode-specific/fundamental)
(require 'conf/mode-specific/javascript)
(require 'conf/mode-specific/web)
(require 'conf/mode-specific/c-and-c++)
(require 'conf/mode-specific/cmake)
(require 'conf/mode-specific/clojure)
(require 'conf/mode-specific/calc)
(require 'conf/mode-specific/compilation)
(require 'conf/mode-specific/org)
(require 'conf/mode-specific/vimrc)
(require 'conf/mode-specific/elisp-and-interaction)
(require 'conf/mode-specific/lisps)
(require 'conf/mode-specific/latex)
(require 'conf/mode-specific/scala)
(require 'conf/mode-specific/sgml)
(require 'conf/mode-specific/pascal)
(require 'conf/mode-specific/puppet)
(require 'conf/mode-specific/asciidoc)
(require 'conf/mode-specific/term)
(require 'conf/mode-specific/bat-cmd)

(provide 'conf/main)
