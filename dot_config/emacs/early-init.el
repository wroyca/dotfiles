; -*- mode: emacs-lisp; lexical-binding: t -*-

(defconst dotemacs--mouse-scroll-amount '(3 ((shift) . 3) ((control) . nil))
  "Amount to scroll for various mouse actions.")

(defconst dotemacs--inhibit-startup t
  "Inhibit startup screen.")

(defconst dotemacs--scratch-buffer-name "*scratch*"
  "Sratch buffer name.")

(defconst dotemacs--gui-elements '(menu-bar-mode scroll-bar-mode tool-bar-mode)
  "List of GUI elements.")

;;

(defun interface ()
  (mapc (lambda (mode) (funcall mode -1)) dotemacs--gui-elements)
  ;; don't display a mode line.
  (setq-default mode-line-format nil))

(defun mouse ()
  (xterm-mouse-mode 1)
  (setq mouse-wheel-scroll-amount dotemacs--mouse-scroll-amount)
  (setq mouse-wheel-progressive-speed nil))

(defun startup ()
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-echo-area-message (user-login-name)
        inhibit-startup-screen dotemacs--inhibit-startup)
  (put 'inhibit-startup-echo-area-message 'saved-value t)
  ;; When Inhibiting the startup screen, Emacs typically displays the *scratch*
  ;; buffer, which has a custom message.
  (setq initial-scratch-message nil))

;;

(interface)
(mouse)
(startup)