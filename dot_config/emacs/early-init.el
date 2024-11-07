;; [[file:README.org::*Early Initialization][Early Initialization:1]]
;; early-init.el --- -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-screen t)
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq initial-scratch-message nil)

;; https://github.com/doomemacs/doomemacs/blob/6a8c09f01288f1ed00a7cc2b7f5887e8f2b4be77/lisp/doom-start.el#L103
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(load-theme 'modus-operandi)
;; Early Initialization:1 ends here

;; [[file:README.org::*Operating System Command (OSC)][Operating System Command (OSC):1]]
(defun dotemacs//xterm-change-text-background (&rest _args)
  (send-string-to-terminal
   (format "\e]11;%s\a" (frame-parameter nil 'background-color))))

(advice-add #'load-theme :after #'dotemacs//xterm-change-text-background)
(advice-add #'consult-theme :after #'dotemacs//xterm-change-text-background)

(add-hook 'resume-tty-functions #'dotemacs//xterm-change-text-background)
(dotemacs//xterm-change-text-background)
;; Operating System Command (OSC):1 ends here

;; [[file:README.org::*Operating System Command (OSC)][Operating System Command (OSC):2]]
(defun dotemacs//xterm-reset-text-background (&rest _args)
  (send-string-to-terminal "\e]111;\a"))

(add-hook 'kill-emacs-hook #'dotemacs//xterm-reset-text-background)
(add-hook 'suspend-tty-functions #'dotemacs//xterm-reset-text-background)
;; Operating System Command (OSC):2 ends here
