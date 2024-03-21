;; -*- lexical-binding: t -*-
;;
;; Copyright 2024 William Roy
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Integration with use-package
;;
;; use-package is a macro that provides convenient syntactic sugar for many
;; common tasks related to installing and configuring Emacs packages. Of
;; course, it does not actually install the packages, but instead defers to a
;; package manager, like straight.el (which comes with use-package integration
;; by default).
;;
(straight-use-package 'use-package)

;;
;; Magit is an interface to the version control system Git, implemented as an
;; Emacs package.
;;
(use-package magit
  :straight (magit :type git :host github :repo "magit/magit"))
;; This package displays keyword entries from source code comments and Org files in the Magit status buffer.
;;
(use-package magit-todos
  :straight (magit-todos :type git :host github :repo "alphapapa/magit-todos")
  :after magit)
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
;;
(use-package forge
  :straight (forge :type git :host github :repo "magit/forge")
  :after magit)
