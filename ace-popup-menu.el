;;; ace-popup-menu.el --- Replace GUI popups -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/ace-popup-menu
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (avy "0.2.0"))
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

;;; Code:

(require 'avy)
(require 'cl-lib)

(defun ace-popup-menu--insert-strings (items)
  "Insert ITEMS much like `completion--insert-strings' in current buffer.

ITEMS should be a list, where every element is a cons of
form (STRING . VALUE), where STRING is the string to be printed
in current buffer and VALUE is used to construct result value of
this function.  ITEMS can contain plain strings, in this case
they are printed with shadow face.  Empty strings are not
printed, instead they begin new sub-section.

Return alist of values (POS . VALUE), where POS indicates
position of STRING in the buffer and VALUE is its associated
value according to ITEMS."
  (when (consp items)
    (let* ((strings (mapcar (lambda (x)
                              (if (consp x)
                                  (car x)
                                x))
                            items))
           (length (apply 'max
                          (mapcar (lambda (s)
                                    (if (consp s)
                                        (+ (string-width (car s))
                                           (string-width (cadr s)))
                                      (string-width s)))
                                  strings)))
           (window (get-buffer-window (current-buffer) 0))
           (wwidth (if window (1- (window-width window)) 79))
           (columns (min (max 2 (/ wwidth (+ 2 length)))
                         (max 1 (/ (length strings) 2))))
           (colwidth (/ wwidth columns))
           (column 0)
           (rows (/ (length strings) columns))
           (row 0)
           (first t)
           laststring
           result)
      (dolist (str strings)
        (unless (equal laststring str)
          (setq laststring str)
          (let ((length (if (consp str)
                            (+ (string-width (car str))
                               (string-width (cadr str)))
                          (string-width str))))
            (cond
             ((eq completions-format 'vertical)
              (when (> row rows)
                (forward-line (- -1 rows))
                (setq row 0 column (+ column colwidth)))
              (when (> column 0)
                (end-of-line)
                (while (> (current-column) column)
                  (if (eobp)
                      (insert "\n")
                    (forward-line 1)
                    (end-of-line)))
                (insert " \t")
                (set-text-properties (1- (point)) (point)
                                     `(display (space :align-to ,column)))))
             (t
              (unless first
                (if (< wwidth (+ (max colwidth length) column))
                    (progn (insert "\n") (setq column 0))
                  (insert " \t")
                  (set-text-properties (1- (point)) (point)
                                       `(display (space :align-to ,column)))
                  nil))))
            (setq first nil)
            (if (plusp (length str))
                (let ((value (assq str items)))
                  (when value
                    (push (cons (point) value) result))
                  (insert (if value str (propertize str 'face 'shadow))))
              (insert "\n\n")  ;; this needs more attention
              (setq column 0)) ;; ^
            (cond
             ((eq completions-format 'vertical)
              (if (> column 0)
                  (forward-line)
                (insert "\n"))
              (setq row (1+ row)))
             (t
              (setq column (+ column
                              (* colwidth (ceiling length colwidth)))))))))
      result)))

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
    (let ((buffer (get-buffer-create "*Ace Popup Menu*"))
          menu-item-alist)
      (with-displayed-buffer-window
       ;; buffer or name
       buffer
       ;; action (for `display-buffer')
       (cons 'display-buffer-below-selected
             '((window-height . fit-window-to-buffer)
               (preserve-size . (nil . t))))
       ;; quit-function
       (lambda (window _value)
         (with-selected-window window
           (unwind-protect
               (list
                (cdr
                 (assq
                  (avy--with-avy-keys ace-popup-menu
                    (avy--process (mapcar #'car menu-item-alist)
                                  #'avy--overlay-pre))
                  menu-item-alist)))
             (when (window-live-p window)
               (quit-restore-window window 'kill)))))
       ;; Here we generate the menu. Currently MENU cannot be a keymap or
       ;; list of keymaps. Support for this representation of MENU will be
       ;; added on request later.
       (with-current-buffer buffer
         (cl-destructuring-bind (title . panes) menu
           (insert (propertize title 'face 'font-lock-function-name-face)
                   "\n\n")
           (dolist (pane panes)
             (cl-destructuring-bind (title . items) pane
               (insert (propertize title 'face 'underline)
                       "\n\n")
               (setq menu-item-alist
                     (ace-popup-menu--insert-strings items))))))))))

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
