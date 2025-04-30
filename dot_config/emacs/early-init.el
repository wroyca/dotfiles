;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; 


;;; Code:

(setq package-enable-at-startup nil)

;;

(load-file (expand-file-name "lisp/dotemacs-appearance.el" user-emacs-directory))
(load-file (expand-file-name "lisp/dotemacs-appearance-control-sequences.el" user-emacs-directory))

;;

(provide 'early-init)

;;; early-init.el ends here

