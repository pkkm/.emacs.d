# My Emacs configuration

I like modal editing and Vim's keybindings. I also really like Lisp and the unparalleled extensibility that it enables in Emacs. This is my attempt to combine (what I consider to be) the advantages of both editors.

Note that it is heavily customized to fit my particular use-cases; for example, the keybindings are optimized for the [Programmer Dvorak](http://www.kaufmann.no/roland/dvorak/) keyboard layout. You probably don't want to use this configuration verbatim. Instead, copy parts of it into your own.

## Compatibility

Requires **Emacs 24.3** or newer.

## Installation

Just clone this repository into `~/.emacs.d`. On the first launch, Emacs will download and install all the necessary packages into `~/.emacs.d/elpa`.

## Features

Some of the used packages are:
* `evil`
  by far the best Vim emulation (that I know of). Elegant code. More customizable than Vim itself.
* `ido`, `ido-ubiquitous`, `smex`, `flx-ido`
  fuzzy completion in the minibuffer.
* `smartparens`
  provides shortcuts for manipulating entire s-expressions at once. Less strict than `paredit`, and its usefulness is not limited to Lisps.
* `auto-complete`
  automatic completion menu when typing. I use it over `company-mode` mostly because of inertia.
* `yasnippet`
  a snippet/template system.
