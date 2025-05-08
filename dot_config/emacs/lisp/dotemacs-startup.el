;;; dotemacs-startup.el --- dotemacs bootstrap loader hijack -*- lexical-binding: t -*-

;;; Commentary:
;; This module intercepts Emacs’ initfile loader to enforce a strict
;; configuration path.  It prevents loading of any unintended user
;; initfiles (e.g., `~/.emacs', `~/.emacs.d/init.el'), and offers
;; controlled delegation to `dotemacs-startup-user-init-file'.

;;; Code:

(defgroup dotemacs-startup nil
  "Bootstrap-time loader dispatch for the dotemacs runtime environment.

This group provides configuration options for intercepting Emacs' native
startup loader and rerouting control to a precise initfile, bypassing
legacy fallbacks and suppressing redundant I/O."
  :group 'initialization
  :prefix "dotemacs-startup-")

(defcustom dotemacs-startup-user-init-file
  (expand-file-name "init.el" user-emacs-directory)
  "Filesystem path to the primary user initialization entrypoint.

This file is loaded in place of legacy paths such as `~/.emacs',
`~/.emacs.el', and `~/.emacs.d/init.el' during Emacs startup.  It must
be a valid readable file or startup will abort gracefully."
  :type 'file
  :group 'dotemacs-startup)

(defun dotemacs--startup-override-init-loader (orig-fn &rest args)
  "Override `startup--load-user-init-file' to enforce
`dotemacs-startup-user-init-file'.

This disables Emacs' legacy initfile discovery logic, which otherwise
attempts to locate multiple fallbacks such as `~/.emacs' or
platform-specific alternatives. If the file does not exist, invoke
`dotemacs--startup-fatal-error'.

ORIG-FN and ARGS are ignored to ensure the override remains consistent
across Emacs versions and does not regress if internals change."
  (condition-case-unless-debug err
    (load dotemacs-startup-user-init-file 'noerror 'nomessage)
    (error
      (dotemacs--startup-fatal-error err))))

(defun dotemacs--startup-fatal-error (error-data)
  "Abort startup and present a fatal bootstrap error screen.

ERROR-DATA must be a caught signal from `condition-case'.  This function
constructs a buffer titled `*dotemacs fatal error*' and displays the
full error context for user debugging."
  (let ((buffer (get-buffer-create "*dotemacs fatal error*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize "dotemacs startup failure\n\n" 'face 'error))
      (insert (format "Unable to load `%s'.\n\n"
                dotemacs-startup-user-init-file))
      (insert "The following error was caught during load:\n\n")
      (insert (format "%s\n\n" (error-message-string error-data)))
      (insert "Aborting startup. Please verify that your dotemacs config is intact.\n")
      (special-mode))
    (switch-to-buffer buffer)
    (error "Startup aborted via `dotemacs--startup-fatal-error'")))

;;;###autoload
(defun dotemacs-startup-init ()
  "Hijack Emacs' initfile resolution and enforce a strict bootstrap target.

This function must be invoked during `early-init.el' or as early as
possible, prior to Emacs resolving `user-init-file'. It forcibly
replaces `startup--load-user-init-file' with a deterministic loader that
consults only `dotemacs-startup-user-init-file'."
  (advice-add #'startup--load-user-init-file
    :override #'dotemacs--startup-override-init-loader))

(provide 'dotemacs-startup)

;;; dotemacs-startup.el ends here
