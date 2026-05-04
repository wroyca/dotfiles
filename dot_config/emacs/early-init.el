;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Early initialization setup.
;;

;;; Code:

;; Bump the garbage collection threshold to the maximum possible
;; value. We are about to allocate a lot of objects during startup,
;; and pausing for GC right now would just slow us down. We will
;; restore this to a more reasonable default later in the init
;; process.
;;
(setq gc-cons-threshold most-positive-fixnum)

;; Take full control over the package loading process. If we allow the
;; default behavior, Emacs will eagerly try to initialize installed
;; packages before we even get a chance to configure our package
;; manager.
;;
(setq package-enable-at-startup nil)

;; The built-in load function is quite chatty, printing "Loading..." for
;; every single file. Since we are about to load a significant portion of
;; our configuration, let's silence it. We do this by advising `load` to
;; temporarily inhibit messages.
;;
(advice-add 'load :around
  (lambda (f &rest a)
    (let ((inhibit-message t))
      (apply f a))))

;; Strip away the visual chrome (menus, toolbars, scrollbars) as early as
;; possible. We push these settings to the default frame alist to prevent
;; visual flashing before the first frame is even created.
;;
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Hide the mode-line entirely for now. We will likely replace it with a
;; custom one later, so there is no point in rendering the default one
;; during startup.
;;
(setq-default mode-line-format nil)

;;; early-init.el ends here
