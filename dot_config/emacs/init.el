; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun +recursive-load-path (path)
  "Recursively load subdirectories in PATH."
  (let* ((path (expand-file-name path user-emacs-directory))
         (local-pkgs (mapcar 'file-name-directory
                             (directory-files-recursively path"\\.el$"))))
    (if (file-accessible-directory-p path)
        (mapc (apply-partially 'add-to-list 'load-path) local-pkgs))))

(dolist (path '("lisp"))
  (+recursive-load-path path))

(require 'packages-elpaca)
(require 'packages-vertico)
(require 'packages-marginalia)

(elpaca-wait)
