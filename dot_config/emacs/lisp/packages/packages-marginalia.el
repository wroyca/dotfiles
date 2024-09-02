; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package marginalia
             :after
             vertico
             :hook
             (vertico-mode . marginalia-mode))

(provide 'packages-marginalia)
