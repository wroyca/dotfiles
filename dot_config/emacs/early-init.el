;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotemacs//before-init-hook (&rest _args)
  ;; Ensure package requests are not preempted by the dependencies they rely on.
  (setq package-enable-at-startup nil)
  ;; Prevent premature redisplay.
  (setq-default inhibit-redisplay t))

(add-hook 'before-init-hook 'dotemacs//before-init-hook)
