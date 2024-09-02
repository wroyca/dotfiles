; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package magit
             :custom
             (magit-commit-diff-inhibit-same-window t)
             (magit-save-repository-buffers 'dontask)
             :hook
             (window-setup . evil-mode))

(provide 'packages-magit)
