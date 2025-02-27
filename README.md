# Ace Popup Menu

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ace-popup-menu-badge.svg)](https://melpa.org/#/ace-popup-menu)
[![CI](https://github.com/mrkkrp/ace-popup-menu/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/ace-popup-menu/actions/workflows/ci.yaml)

![Ace Popup Menu](https://raw.githubusercontent.com/mrkkrp/ace-popup-menu/gh-pages/ace-popup-menu.png)

This package allows its users to replace the GUI popup menu (created by
`x-popup-menu` by default) with a little textual window. In this window,
menu items are displayed and labeled with one or two letters.

## Installation

The package is available via MELPA, so you can just type `M-x
package-install RET ace-popup-menu RET`.

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`. Then you can require it in your init file like
this:

```emacs-lisp
(require 'ace-popup-menu)
```

## Usage

In order to replace the standard behavior of `x-popup-menu`, activate
`ace-popup-menu-mode` in your configuration file, like this:

```emacs-lisp
(ace-popup-menu-mode 1)
```

You can toggle/activate it either interactively or programmaticaly. The mode
follows all usual Emacs Lisp conventions for minor modes, except it's always
global. See the documentation for `ace-popup-menu-mode` for more
information.

You can use the enhanced menu directly via `ace-popup-menu`, too. To use it
you don't need to enable the minor mode. See documentation of the function
for detailed information.

## With Flyspell

A popular use case for this package is in conjunction with Flyspell. I
personally use the following function for quick correction of misspellings:

```emacs-lisp
(defun mk-flyspell-correct-previous (&optional words)
  "Correct word before point, reach distant words.

WORDS words at maximum are traversed backward until a misspelled
word is found.  If the argument WORDS is not specified, traverse
12 words by default.

Return T if a misspelled word is found and NIL otherwise.  Never
move the point."
  (interactive "P")
  (let* ((delta (- (point-max) (point)))
         (counter (string-to-number (or words "12")))
         (result
          (catch 'result
            (while (>= counter 0)
              (when (cl-some #'flyspell-overlay-p
                             (overlays-at (point)))
                (flyspell-correct-word-before-point)
                (throw 'result t))
              (backward-word 1)
              (setq counter (1- counter))
              nil))))
    (goto-char (- (point-max) delta))
    result))
```

It works nicely with `ace-popup-menu-mode` enabled.

## Customization

You can ask Ace Popup Menu to show headers of individual panes (they are not
shown in the original GUI popup menu):

```emacs-lisp
(setq ace-popup-menu-show-pane-header t)
```

This variable can be changed via the customization interface as well.

This package is built on top of
[`avy-menu`](https://github.com/mrkkrp/avy-menu), see its customization
settings if you wish to change appearance of the menu itself.

## Limitations

Here is something you may want to know:

* The original `x-popup-menu` can take `menu` argument in the form of a
  keymap or a list of keymaps. This is currently not supported. If you run
  into a situation when this breaks something, please [open an
  issue](https://github.com/mrkkrp/ace-popup-menu/issues). Describe how to
  reproduce your problem and I'll try to fix it.

* Some packages, such as `flyspell`, may test if they work under X-window
  system and refuse to call `x-popup-menu` if they think it's unavailable.
  There is nothing I can do about it, so it may be hard to use Ace Popup
  Menu in terminal, even though it's perfectly capable of functioning there.

* Currently only horizontal format of menu items is available. This is
  because it's much easier to implement. This should not be a problem,
  though.

## License

Copyright © 2015–present Mark Karpov

Distributed under GNU GPL, version 3.
