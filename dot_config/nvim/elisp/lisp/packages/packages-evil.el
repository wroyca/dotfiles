; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package evil
             :init
             (setq evil-want-keybinding nil)
             :custom
             (evil-default-state 'normal)
             :hook
             (window-setup . evil-mode))

(provide 'packages-evil)
