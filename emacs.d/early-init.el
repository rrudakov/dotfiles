;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Roman Rudakov

;; Author: Roman Rudakov <phentagram@gmail.com>
;; Keywords: early-init startup configuration

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
