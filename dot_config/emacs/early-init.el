;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(setq package-enable-at-startup nil)

(defadvice load (around quiet-loading activate)
  "Silence loading messages during file loading operations."
  (let ((inhibit-message t))
    ad-do-it))

(mapc (lambda (mode) (funcall mode -1))
  '(menu-bar-mode scroll-bar-mode tool-bar-mode fringe-mode))
(setq-default mode-line-format nil)

;;; early-init.el ends here
