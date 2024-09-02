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
(require 'packages-evil)
(require 'packages-evil-collection)
(require 'packages-magit)
(require 'packages-forge)
(require 'packages-kkp)
(require 'packages-clipetty)

(elpaca-wait)

(defun no-op ()
  "A no-op function for use with key bindings."
  (interactive))

(defun parse-command-line (args)
  "Handle specific command line arguments.
The reason why we don't use the Emacs hooks for processing user defined
arguments is that we want to process these arguments as soon as possible."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
          ("--magit"
           (evil-collection-init 'magit)
           (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
           (evil-define-key 'normal magit-status-mode-map (kbd "q") 'no-op)
           (magit-status))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args))
  (delete-other-windows))

(setq command-line-args (parse-command-line command-line-args))

