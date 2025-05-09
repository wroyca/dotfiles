;;; dotemacs-appearance.el --- System theme integration via D-Bus -*- lexical-binding: t -*-

;;; Commentary:
;; This module provides first-class synchronization between Emacs and
;; the system's appearance settings as exposed via the freedesktop.org
;; D-Bus appearance portal.
;;
;; NOTE: It strictly avoids probing or inspecting Emacs' theme state
;; beyond its own scope. That is, it does not attempt to infer the
;; current theme visually or restore a previous state across
;; sessions. Instead, it operates as a deterministic, event-driven
;; interface responding to system notifications with minimal side
;; effects.

;;; Code:

(require 'dbus)

(defgroup dotemacs-appearance nil
  "System appearance synchronization for Emacs."
  :group 'faces
  :prefix "dotemacs-appearance-")

(defcustom dotemacs-appearance-light-theme 'modus-operandi-tinted
  "Theme to use when system is using light mode."
  :type 'symbol
  :group 'dotemacs-appearance)

(defcustom dotemacs-appearance-dark-theme 'modus-vivendi-tinted
  "Theme to use when system is using dark mode."
  :type 'symbol
  :group 'dotemacs-appearance)

(defconst dotemacs-appearance--color-scheme-light 2
  "Value representing light color scheme from freedesktop portal.")

(defconst dotemacs-appearance--color-scheme-dark 1
  "Value representing dark color scheme from freedesktop portal.")

(defconst dotemacs-appearance--color-scheme-default 0
  "Value representing default color scheme from freedesktop portal.

NOTE: This value typically implies light color scheme, but may be treated as ambiguous.")

(defvar dotemacs-appearance--current-theme nil
  "Currently active theme set by appearance module.")

;;;###autoload
(defun dotemacs-appearance-parse-color-scheme (value)
  "Apply appropriate theme based on system color scheme VALUE.

VALUE is an integer as defined by the freedesktop.org appearance portal:
- 0: Default (usually implies light preference, see above.)
- 1: Dark preference
- 2: Light preference"
  (let ((theme (cond
                ((or (= value dotemacs-appearance--color-scheme-light)
                     (= value dotemacs-appearance--color-scheme-default))
                 (prog1 dotemacs-appearance-light-theme
                   (set-frame-parameter nil 'background-mode 'light)))
                (t
                 (prog1 dotemacs-appearance-dark-theme
                   (set-frame-parameter nil 'background-mode 'dark))))))
    ;; Clean transition: unload previous theme only if different from target
    (when (and dotemacs-appearance--current-theme
               (not (eq dotemacs-appearance--current-theme theme))
               (custom-theme-enabled-p dotemacs-appearance--current-theme))
      (disable-theme dotemacs-appearance--current-theme))
    (unless (custom-theme-enabled-p theme)
      (load-theme theme t))
    (setq dotemacs-appearance--current-theme theme)
    ;; Ensure background-mode propagates to all frames
    (frame-set-background-mode nil)))

;;;###autoload
(defun dotemacs-appearance-setup-sync ()
  "Establish D-Bus connections for system appearance monitoring.
Creates two communication channels with the desktop portal:
1. Initial value retrieval to sync current system state
2. Signal registration for change notifications"
  (if (not (featurep 'dbusbind))
      (message "D-Bus support not available, cannot sync system appearance")
    (condition-case err
        (progn
          ;; Asynchronously retrieve current system preference to establish initial state
          (dbus-call-method-asynchronously
           :session
           "org.freedesktop.portal.Desktop"
           "/org/freedesktop/portal/desktop"
           "org.freedesktop.portal.Settings"
           "Read"
           (lambda (value)
             (when value
               (dotemacs-appearance-parse-color-scheme (car (car value)))))
           "org.freedesktop.appearance"
           "color-scheme")

          ;; Monitor for system preference changes in real-time
          (dbus-register-signal
           :session
           "org.freedesktop.portal.Desktop"
           "/org/freedesktop/portal/desktop"
           "org.freedesktop.portal.Settings"
           "SettingChanged"
           (lambda (namespace key variant)
             (when (and (string-equal namespace "org.freedesktop.appearance")
                        (string-equal key "color-scheme"))
               (let ((color-scheme (car variant)))
                 (when color-scheme
                   (dotemacs-appearance-parse-color-scheme color-scheme)))))))
      (error
       (message "Failed to set up appearance synchronization: %s" (error-message-string err))))))

;;;###autoload
(defun dotemacs-appearance-toggle-theme ()
  "Toggle between light and dark themes manually."
  (interactive)
  (if (eq dotemacs-appearance--current-theme dotemacs-appearance-light-theme)
      (dotemacs-appearance-parse-color-scheme dotemacs-appearance--color-scheme-dark)
    (dotemacs-appearance-parse-color-scheme dotemacs-appearance--color-scheme-light)))

;;;###autoload
(define-minor-mode dotemacs-appearance-sync-mode
  "Toggle automatic synchronization with system appearance.
When enabled, Emacs will switch between light and dark themes
based on the system appearance settings."
  :global t
  :init-value nil
  :lighter " Appearance Sync"
  (if dotemacs-appearance-sync-mode
      (dotemacs-appearance-setup-sync)
    (when dotemacs-appearance--current-theme
      (message "Appearance sync disabled, theme remains %s" dotemacs-appearance--current-theme))))

(dotemacs-appearance-sync-mode 1)

(provide 'dotemacs-appearance)

;;; dotemacs-appearance.el ends here
