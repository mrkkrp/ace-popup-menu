# Ace Popup Menu

*This stuff is under development. It may not work yet.*

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/ace-popup-menu.svg?branch=master)](https://travis-ci.org/mrkkrp/ace-popup-menu)

This package allows to replace GUI popup menus (created by `x-popup-menu` by
default) with little temporary windows (like those in which Dired shows you
files you want to copy). In these windows menu items are displayed and
labeled with one or two letters. You press a key corresponding to desired
choice (or C-g) and you are done.

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'ace-popup-menu)` into your configuration file. Done!

## Usage

In order to replace standard behavior of `x-popup-menu`, activate
`ace-popup-menu-mode` in your configuration file, like this:

```
(ace-popup-menu-mode 1)
```

You can disable it too either interactively or via Lisp, the mode follows
all usual Emacs Lisp conventions for minor modes, except it's always global
(because it doesn't make any sense to replace behavior of `x-popup-menu`
only for in some specific buffer). See documentation for
`ace-popup-menu-mode` for more information.

----

You can use the enhanced menu directly via `ace-popup-menu-mode` too. To use
it you don't need to enable the minor mode. See documentation of the
function for detailed information.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
