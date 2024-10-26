;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotemacs//before-init-hook (&rest _args)
  ;; Ensure package requests are not preempted by the dependencies they rely on.
  (setq package-enable-at-startup nil)

  ;; We run this in `before-init-hook' instead of `tty-setup-hook'
  ;; because the latter happens too late.
  (mapc (lambda (mode) (funcall mode -1))
	'(menu-bar-mode scroll-bar-mode tool-bar-mode))
  (setq-default mode-line-format nil))

(add-hook 'before-init-hook 'dotemacs//before-init-hook)
