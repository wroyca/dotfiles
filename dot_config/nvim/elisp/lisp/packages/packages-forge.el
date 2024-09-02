; -*- mode: emacs-lisp; lexical-binding: t -*-

(use-package forge
             :after
             (magit transient)
             :preface
             (setq forge-add-default-bindings nil))

(provide 'packages-forge)
