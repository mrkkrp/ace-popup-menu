# Ace Popup Menu

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/ace-popup-menu.svg?branch=master)](https://travis-ci.org/mrkkrp/ace-popup-menu)

This package allows to replace GUI popup menus (created by `x-popup-menu` by
default) with little temporary windows (like those in which Dired shows you
files you want to copy). In these windows menu items are displayed and
labeled with one or two letters. You press a key corresponding to desired
choice (or <kbd>C-g</kbd>) and you are done.

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'ace-popup-menu)` into your configuration file. Done!

## Usage

In order to replace standard behavior of `x-popup-menu`, activate
`ace-popup-menu-mode` in your configuration file, like this:

```
(ace-popup-menu-mode 1)
```

You can toggle/activate it either interactively or via Lisp, the mode
follows all usual Emacs Lisp conventions for minor modes, except it's always
global (because it doesn't make any sense to replace behavior of
`x-popup-menu` only for in some specific buffer). See documentation for
`ace-popup-menu-mode` for more information.

----

You can use the enhanced menu directly via `ace-popup-menu-mode` too. To use
it you don't need to enable the minor mode. See documentation of the
function for detailed information.

## Customization

To save space you may want to remove headers of individual panes in the
menu (they are not shown in original GUI popup menu at all):

```
(setq ace-popup-menu-show-pane-header nil)
```

This variable can be changed via «customize» interface too.

## Limitations

Here is something you may want to know:

* Original `x-popup-menu` can take `menu` argument in form of keymap or list
  of keymaps. This is currently not supported. If you encounter a situation
  when this breaks something, please
  [open an issue](https://github.com/mrkkrp/ace-popup-menu/issues). Describe
  how to reproduce your problem and I'll fix it.

* Some packages, like `flyspell` may test if they work under X-window system
  and refuse to call `x-popup-menu` if they think it's unavailable. There is
  nothing I can do about it, so it may be hard to use Ace Popup Menu in
  terminal, even though it's perfectly capable of functioning there.

* Currently only horizontal format of menu items is available. This is
  because it's much easier to implement. This should not be a problem,
  though.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
