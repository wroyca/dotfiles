; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package vertico
             :hook
             (elpaca-after-init . vertico-mode)
             (vertico-mode . vertico-mouse-mode))

(provide 'packages-vertico)
