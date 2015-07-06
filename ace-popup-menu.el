;;; ace-popup-menu.el --- Replace GUI popups -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/ace-popup-menu
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (avy "0.2.0"))
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

;; This package allows to replace GUI popup menus (created by `x-popup-menu'
;; by default) with little temporary windows (like those in which Dired
;; shows you files you want to copy). In these windows menu items are
;; displayed and labeled with one or two letters. You press a key
;; corresponding to desired choice (or C-g) and you are done.

;;; Code: rere

(require 'avy)

;;;###autoload
(defun ace-popup-menu (position menu)
  "Pop up menu in a temporary window and return user's selection.
Argument POSITION is taken for compatibility and ignored unless
it's NIL, in this case this function has no effect.  For meaning
of MENU argument see description of `x-popup-menu'.

Every selectable item in the menu is labeled with a letter (or
two).  User can press letter corresponding to desired menu item
and he is done."
  (message "saw menu: %s" menu) ;; for investigation…
  (let ((result (prog2
                    (advice-remove 'x-popup-menu #'ace-popup-menu)
                    (x-popup-menu position menu)
                  (advice-add 'x-popup-menu :override #'ace-popup-menu))))
    (message "saw result: %s" result)
    result))

;;;###autoload
(define-minor-mode ace-popup-menu-mode
  "Toggle ace-popup-menu-mode minor mode.
With a prefix argument ARG, enable ace-popup-menu mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'.

This minor mode is global. When it's active any call to
`x-popup-menu' will result in call of `ace-popup-menu'
instead. This function in turn implements more efficient
interface to select an option from a list. Emacs Lisp code can
also use `ace-popup-menu' directly, in this case it will work OK
even if the mode is disabled."
  :global t
  :group 'ace-popup-menu
  (if ace-popup-menu-mode
      (advice-add 'x-popup-menu :override #'ace-popup-menu)
    (advice-remove 'x-popup-menu #'ace-popup-menu)))

(provide 'ace-popup-menu)

;;; ace-popup-menu.el ends here
