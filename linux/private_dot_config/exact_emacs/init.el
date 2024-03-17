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

;; Load our primary theme as early as possible.
;;
(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (nano-light))

;; Load-theme explicit in terminal mode as set-frame-background fails to work properly.
;;
(unless (display-graphic-p)
  (load-theme 'nano t) (xterm-mouse-mode 1))

;; Evil is an extensible vi layer for Emacs. It emulates the main features of Vim,
;; and provides facilities for writing custom extensions. Also see
;; https://www.emacswiki.org/emacs/Evil
;;
(use-package evil
  :straight (evil :type git :host github :repo "emacs-evil/evil"))
(require 'evil) (evil-mode 1)

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

;; By default, Emacs regroups multiple scroll events into a single one large
;; enough to scroll one line. This produces much fewer events with a coarse
;; precision. This goes against the smooth experience sought by
;; pixel-scroll-precision-mode, so it disables this feature by setting
;; mwheel-coalesce-scroll-events to nil.
;;
;; Unfortunately, this affects all wheel events, while
;; pixel-scroll-precision-mode cares only about vertical scrolling. Other
;; wheel-based features go crazy (for instance, scaling text with a mouse wheel
;; is roughly 20 times faster on my setup, quite inconvenient).
;;
;; The tentative fix is to switch coalescing on and off based on the action.
;; https://def.lakaban.net/2023-03-05-high-quality-scrolling-emacs/

(defun filter-mwheel-always-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
   coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (apply orig args)
    (setq mwheel-coalesce-scroll-events t)))

(defun filter-mwheel-never-coalesce (orig &rest args)
  "A filter function suitable for :around advices that ensures only
   non-coalesced scroll events reach the advised function."
  (if mwheel-coalesce-scroll-events
      (setq mwheel-coalesce-scroll-events nil)
    (apply orig args)))

(setopt pixel-scroll-precision-mode t)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-tilt-scroll t)

(setq scroll-conservatively 101)
(setq scroll-margin 4)
(setq scroll-preserve-screen-position t)

; Don't coalesce for high precision scrolling
;;
(advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)

; Coalesce for default scrolling (which is still used for horizontal scrolling)
; and text scaling (bound to ctrl + mouse wheel by default).
;;
(advice-add 'mwheel-scroll          :around #'filter-mwheel-always-coalesce)
(advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)
