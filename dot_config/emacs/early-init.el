;;; early-init.el --- dotemacs early initialization entrypoint -*- lexical-binding: t -*-

;;; Commentary:
;; This file defines the deterministic early-stage loader for
;; dotemacs. It configures runtime-critical variables affecting
;; garbage collection, file resolution behavior, and legacy init
;; suppression before Emacs proceeds to load `user-init-file'.
;;
;; All logic is encapsulated in `dotemacs-early-init', which is
;; invoked with `file-name-handler-alist' temporarily suppressed to
;; reduce I/O overhead.

;;; Code:

(defgroup dotemacs-early-init nil
  "Low-level configuration options governing early-stage loader behavior
for dotemacs.

This group includes garbage collector tuning, initfile suppression
controls, and resolution policy for source versus byte-compiled files."
  :group 'initialization
  :prefix "dotemacs-early-init-")

(defvar dotemacs-early-init-gc-cons-threshold
  most-positive-fixnum
  "Threshold to assign to `gc-cons-threshold' during early startup.

A higher value defers garbage collection, reducing allocation
interruptions during init. This value is expected to be reset at a later
stage in user configuration.")

(defvar dotemacs-early-init-gc-cons-percentage 0.6
  "Value to assign to `gc-cons-percentage' during early startup.

Combined with `dotemacs-early-init-gc-cons-threshold', this suppresses
premature collection events during the initial bootstrap.")

(defvar dotemacs-early-init-disable-package-el t
  "If non-nil, inhibit `package.el' automatic initialization.

This disables legacy `package.el' autoloading behavior to reduce startup
cost and delegate package system initialization to dotemacs core.")

(defvar dotemacs-early-init-disable-site-run-file t
  "If non-nil, inhibit `site-start.el' automatic initialization.

This disable site-wide configurations that may be injected by system
package managers (e.g., those used in distributions like Fedora), which
can introduce unintended or non-standard behavior into Emacs'
environment.")

(defvar dotemacs-early-init-prefer-source t
  "If non-nil, prioritize `.el' over `.elc' during `load' resolution.

This setting disables Emacs' default behavior of preferring
byte-compiled files and can improve traceability during development.")

(defvar dotemacs-early-init-inhibit-default-init t
  "If non-nil, disable loading of `default.el'.

This prevents implicit sourcing of legacy `default.el' behavior,
ensuring a clean and fully deterministic startup environment.")

(defvar dotemacs-early-init-load-suffixes '(".elc" ".el")
  "List of file extensions to consider during `load' resolution.

This defines the suffix priority used by the Emacs `load' mechanism,
typically to reduce file stat calls and enforce source load ordering.")

(defun dotemacs--early-init-optimize-gc ()
  "Apply garbage collector thresholds defined in
`dotemacs-early-init-gc-cons-threshold'
and`dotemacs-early-init-gc-cons-percentage' to delay collection during
startup.

This operation is performance-sensitive and must be performed before any
major memory allocations by user packages or the core system."
  (setq gc-cons-threshold dotemacs-early-init-gc-cons-threshold
        gc-cons-percentage dotemacs-early-init-gc-cons-percentage))

(defun dotemacs--early-init-configure-load-behavior ()
  "Configure load resolution policy for Emacs core during early startup.

This applies `dotemacs-early-init-load-suffixes' to `load-suffixes' and,
if `dotemacs-early-init-prefer-source' is non-nil, sets
`load-prefer-newer' to prioritize `.el' sources over `.elc' files."
  (setq load-suffixes dotemacs-early-init-load-suffixes
        load-prefer-newer dotemacs-early-init-prefer-source))

(defun dotemacs--early-init-disable-defaults ()
  "Disable legacy startup mechanisms that interfere with deterministic
bootstrap.

This sets `package-enable-at-startup', 'site-run-file' and
`inhibit-default-init' according to
`dotemacs-early-init-disable-package-el',
`dotemacs-early-init-disable-site-run-file' and
`dotemacs-early-init-inhibit-default-init', respectively, avoiding
unnecessary loading of obsolete user configuration layers."
  (when dotemacs-early-init-disable-package-el
    (setq package-enable-at-startup nil
      package--init-file-ensured t))
  (when dotemacs-early-init-disable-site-run-file
    (setq site-run-file nil))
  (when dotemacs-early-init-inhibit-default-init
    (setq inhibit-default-init t)))

(defun dotemacs--early-init-appearance ()
  "Load appearance-related modules required during early initialization.

This loads `dotemacs-appearance.el' and
`dotemacs-appearance-control-sequences.el' from the user configuration
directory. These modules synchronize Emacs theme state with system and
terminal background settings."
  (load-file (expand-file-name "lisp/dotemacs-appearance.el" user-emacs-directory))
  (load-file (expand-file-name "lisp/dotemacs-appearance-control-sequences.el" user-emacs-directory)))

(defun dotemacs--early-init-startup ()
  "Load the startup module and invoke `dotemacs-startup-init'.

This validates the presence of `dotemacs-startup.el' in the user
configuration directory and loads it in noninteractive mode. If the
function `dotemacs-startup-init' is defined, it is invoked
immediately."
  (let ((startup-file (expand-file-name "lisp/dotemacs-startup.el" user-emacs-directory)))
    (unless (file-readable-p startup-file)
      (error "Missing required startup module at %s" startup-file))
    (load startup-file nil 'nomessage))
  (when (fboundp 'dotemacs-startup-init)
    (dotemacs-startup-init)))

;;;###autoload
(defun dotemacs-early-init ()
  "Apply early-stage configuration to Emacs runtime.

This function performs three operations in deterministic order:

(1) garbage collector suppression via
`dotemacs--early-init-optimize-gc',

(2) file resolution policy enforcement via
`dotemacs--early-init-configure-load-behavior',

(3) legacy subsystem deactivation via
`dotemacs--early-init-disable-defaults'.

It must be invoked from `early-init.el' with `file-name-handler-alist'
temporarily bound to nil to ensure stateless performance."
  (dotemacs--early-init-optimize-gc)
  (dotemacs--early-init-configure-load-behavior)
  (dotemacs--early-init-disable-defaults)
  (dotemacs--early-init-appearance)
  (dotemacs--early-init-startup))

;; Entrypoint: perform all latency-critical adjustments before init.el
(let (file-name-handler-alist)
  (dotemacs-early-init))

(provide 'early-init)

;;; early-init.el ends here
