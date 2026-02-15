;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

;; We want full control over the package loading process. If we allow the
;; default behavior, Emacs will try to initialize installed packages before we
;; have had a chance to configure our package manager.
;;
(setq package-enable-at-startup nil)

;; The built-in load function is chatty, printing "Loading..." for every file.
;; Since we are about to load a significant portion of our config, We want to
;; temporarily inhibit messages.
;;
(advice-add 'load :around
  (lambda (f &rest a)
    (let ((inhibit-message t))
      (apply f a))))

;;; early-init.el ends here
