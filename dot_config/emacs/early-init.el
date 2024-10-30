;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defvar dotemacs--package-enable-at-startup nil
  "When nil, package requests will not be preempted by the dependencies they rely on.")

(defun dotemacs//before-init-hook (&rest _args)
  (setq package-enable-at-startup dotemacs--package-enable-at-startup)
  (mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (setq-default mode-line-format nil))

(add-hook 'before-init-hook 'dotemacs//before-init-hook)
