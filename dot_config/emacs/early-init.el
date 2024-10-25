;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotemacs//before-init-hook (&rest _args)
  (setq package-enable-at-startup nil))

(add-hook 'before-init-hook 'dotemacs//before-init-hook)
