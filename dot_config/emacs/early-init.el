;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(setq package-enable-at-startup nil)

(set-frame-parameter nil 'undecorated t)
(mapc (lambda (mode) (funcall mode -1))
  '(menu-bar-mode scroll-bar-mode tool-bar-mode fringe-mode))
(setq-default mode-line-format nil)

;;; early-init.el ends here
