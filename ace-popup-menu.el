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

;;; Code:

(require 'avy)

;; write something here, so it works

(provide 'ace-popup-menu)

;;; ace-popup-menu.el ends here
