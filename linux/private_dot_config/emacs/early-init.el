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
(straight-use-package 'use-package)

(use-package doom-themes     :straight (doom-themes     :type git        :host github :repo "doomemacs/themes"))
(use-package vertico         :straight (vertico         :type git        :host github :repo "emacs-straight/vertico"))
(use-package orderless       :straight (orderless       :type git        :host github :repo "oantolin/orderless"))
(use-package marginalia      :straight (marginalia      :type git        :host github :repo "minad/marginalia"))
(use-package magit-todos     :straight (magit-todos     :type git        :host github :repo "alphapapa/magit-todos"))
(use-package forge           :straight (forge           :type git        :host github :repo "magit/forge"))
(use-package evil            :straight (evil            :type git        :host github :repo "emacs-evil/evil"))
(use-package auto-dark       :straight (auto-dark       :type git        :host github :repo "LionyxML/auto-dark-emacs"))
(use-package clipetty        :straight (clipetty        :type git        :host github :repo "spudlyo/clipetty") :ensure t :hook (after-init . global-clipetty-mode))

(setq custom-safe-themes t)
(load-theme 'doom-tomorrow-night)
(auto-dark-mode)
(setopt tool-bar-mode nil menu-bar-mode nil scroll-bar-mode nil)

(evil-mode)
(fset 'evil-visual-update-x-selection 'ignore)

(xterm-mouse-mode)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; HACK: Clipetty isn't functioning correctly when copying from Emacs to the system; however, it works perfectly when copying from the system to Emacs.
;; Conversely, the interprogram snippet below doesn't function when copying from the system to Emacs, but it does when copying from Emacs to the system.
;; Though a bit messy, let's combine both and revisit this whole Wayland clipboard madness another day.
;;
(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (defvar wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function #'wl-copy
        interprogram-paste-function #'wl-paste)
  nil)
