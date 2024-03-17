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

;; vc-use-package has been merged into Emacs master, but Emacs 29 is feature
;; frozen already, so we'll have to wait for Emacs ~30.
;;
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; On GNU/Linux and other systems with case-sensitive file names, Emacs
;; performs a case-sensitive search through auto-mode-alist. If this search
;; fails, it performs a second case-insensitive search through the alist. This
;; is expensive, so suppress the second search.
;;
(setq auto-mode-case-fold nil)

;; This variable controls whether text in the buffer is reordered for display.
;; If its value is non-nil, Emacs reorders characters that have right-to-left
;; directionality when they are displayed.
;;
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
;; ?
(setq bidi-inhibit-bpa nil)

;; Setting the mark in one buffer has no effect on the marks in other buffers.
;; When we return to a buffer with an active mark, the mark is at the same
;; place as before. When multiple windows show the same buffer, they can have
;; different values of point, and thus different regions, but they all share
;; one common mark position. Ordinarily, only the selected window highlights
;; its region; however, if the variable highlight-nonselected-windows is nil,
;; each window highlights its own region.
;;
(setq highlight-nonselected-windows nil)

;; The cursor normally appears in non-selected windows as a non-blinking hollow
;; box—for a bar cursor, it instead appears as a thinner bar; turn off cursors
;; in non-selected windows for performance.
;;
(setq-default cursor-in-non-selected-windows nil)

;; Sometimes, Emacs fails to keep up with the rapid rate of scrolling
;; requested; the display doesn't update and Emacs can become unresponsive to
;; input for quite a long time. We can counter this sluggishness by setting the
;; variable fast-but-imprecise-scrolling to a non-nil value. This instructs the
;; scrolling commands not to fontify any unfontified text they scroll over,
;; instead to assume it has the default face.
;;
(setq fast-but-imprecise-scrolling t)

;; Display characters using large fonts for performance, at the price of a
;; larger memory footprint.
;;
(setq inhibit-compacting-font-caches t)

;; If this variable is non-nil, skip some fontifications if there's input
;; pending. This usually does not affect the display because redisplay is
;; completely skipped anyway if input was pending, but it can make scrolling
;; smoother by avoiding unnecessary fontification.
;;
(setq redisplay-skip-fontification-on-input t)

;; PGTK builds only.
;;
;; This timeout adds latency to frame operations which are frequently called
;; without guard as it's inexpensive in non-PGTK builds. Lowering the timeout
;; from the default 0.1 should make childframes and packages that manipulate
;; them (like `lsp-ui', `company-box', and `posframe') feel much snappier.
;;
;; https://github.com/emacs-lsp/lsp-ui/issues/613
;;
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Hide graphical elements, starting with the frame decoration first.
;;
(setq default-frame-alist '((undecorated . t)))
(push '(menu-bar-lines . 0)     default-frame-alist)
(push '(tool-bar-lines . 0)     default-frame-alist)
(push '(vertical-scroll-bars)   default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
