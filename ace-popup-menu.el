;;; ace-popup-menu.el --- Replace GUI popup menu with something more efficient -*- lexical-binding: t; -*-
;;
;; Copyright © 2015–2016 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/ace-popup-menu
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3") (avy-menu "0.1"))
;; Keywords: convenience, popup, menu
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to replace GUI popup menu (created by `x-popup-menu'
;; by default) with little temporary window (like that in which Dired shows
;; you files you want to copy).  In this window, menu items are displayed
;; and labeled with one or two letters.  You press a key corresponding to
;; desired choice (or C-g) and you are done.

;;; Code:

(require 'avy-menu)
(require 'cl-lib)

(defgroup ace-popup-menu nil
  "Replace GUI popup menu with something more efficient."
  :group  'convenience
  :tag    "Ace Popup Menu"
  :prefix "ace-popup-menu-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/ace-popup-menu"))

(defcustom ace-popup-menu-show-pane-header nil
  "Whether to print headers of individual panes in Ace Popup Menu."
  :tag "Show Pane Header"
  :type 'boolean)

;;;###autoload
(define-minor-mode ace-popup-menu-mode
  "Toggle `ace-popup-menu-mode' minor mode.

With a prefix argument ARG, enable `ace-popup-menu mode' if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode is global. When it's active any call to
`x-popup-menu' will result in call of `ace-popup-menu'
instead. This function in turn implements more efficient
interface to select an option from a list. Emacs Lisp code can
also use `ace-popup-menu' directly, in this case it will work OK
even if the mode is disabled."
  :global t
  (if ace-popup-menu-mode
      (advice-add 'x-popup-menu :override #'ace-popup-menu)
    (advice-remove 'x-popup-menu #'ace-popup-menu)))

;;;###autoload
(defun ace-popup-menu (position menu)
  "Pop up menu in a temporary window and return user's selection.

Argument POSITION is taken for compatibility and ignored unless
it's NIL, in this case this function has no effect.  For meaning
of MENU argument see description of `x-popup-menu'.

Every selectable item in the menu is labeled with a letter (or
two).  User can press letter corresponding to desired menu item
and he is done."
  (when position
    (avy-menu "*ace-popup-menu*"
              menu
              ace-popup-menu-show-pane-header)))

(provide 'ace-popup-menu)

;;; ace-popup-menu.el ends here
