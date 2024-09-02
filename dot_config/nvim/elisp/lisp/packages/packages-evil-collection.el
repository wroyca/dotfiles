; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package evil-collection
             :after
             evil
             :hook
             (evil-mode . evil-collection-init))

(provide 'packages-evil-collection)
