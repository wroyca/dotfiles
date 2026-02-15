;;; dotemacs-context-menu.el --- Context menu handler that respects inhibition -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides a mechanism to inhibit `context-menu-mode' under
;; specific conditions, such as during help command execution or active
;; completion sessions (e.g., Marginalia/Vertico).
;;
;; For additional context, see the bug report below. Be advised that,
;; while it contains relevant information, it also hosts an impressive
;; display of off-topic bikeshedding that manages to thoroughly derail
;; the thread into utter nonsense:
;;
;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2022-02/msg00779.html

;;; Code:

(require 'mouse)

(defgroup dotemacs-context-menu nil
  "Context menu handler that respects inhibition."
  :group 'mouse
  :group 'completion
  :prefix "dotemacs-context-menu-")

(defcustom dotemacs-context-menu-inhibit-during-help t
  "Non-nil means inhibit context menu during help functions.
Affects `describe-key', `describe-function' and `describe-variable'."
  :type 'boolean
  :group 'dotemacs-context-menu)

(defcustom dotemacs-context-menu-inhibit-during-marginalia t
  "Non-nil means inhibit context menu during Marginalia annotation.
Prevents mouse wheel conflicts in Vertico completion."
  :type 'boolean
  :group 'dotemacs-context-menu)

(defvar dotemacs-context-menu--inhibit-context-menu nil
  "Non-nil inhibits context menu creation.
Used internally to prevent completion framework conflicts.")

(defvar dotemacs-context-menu--original-context-menu-map nil
  "Original `context-menu-map' function.")

(defun dotemacs-context-menu//inhibit-wrapper (orig-fn &rest args)
  "Advice wrapper to temporarily inhibit context menu.
Calls ORIG-FN with ARGS while `dotemacs-context-menu--inhibit-context-menu' is t."
  (let ((dotemacs-context-menu--inhibit-context-menu t))
    (apply orig-fn args)))

(defun dotemacs-context-menu//context-menu-map-replacement (&optional click)
  "Replacement `context-menu-map' with inhibition support.
Optional CLICK is the mouse event. Returns nil when inhibited."
  (if dotemacs-context-menu--inhibit-context-menu
      nil
    (if dotemacs-context-menu--original-context-menu-map
        (funcall dotemacs-context-menu--original-context-menu-map click)
      (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t)))
            (click (or click last-input-event)))
        (run-hook-with-args-until-success 'context-menu-functions menu click)
        (when (and (not (mouse-posn-property (event-start click) 'context-menu-p))
                   (functionp context-menu-filter-function))
          (setq menu (funcall context-menu-filter-function menu click)))
        menu))))

(defun dotemacs-context-menu//setup-help-advice ()
  "Add advice to help functions for context menu inhibition."
  (when dotemacs-context-menu-inhibit-during-help
    (advice-add 'describe-key :around #'dotemacs-context-menu//inhibit-wrapper)
    (advice-add 'describe-function :around #'dotemacs-context-menu//inhibit-wrapper)
    (advice-add 'describe-variable :around #'dotemacs-context-menu//inhibit-wrapper)))

(defun dotemacs-context-menu//setup-marginalia-advice ()
  "Add advice to Marginalia functions for context menu inhibition."
  (with-eval-after-load 'marginalia
    (when (and dotemacs-context-menu-inhibit-during-marginalia
              (featurep 'marginalia))
      (advice-add 'marginalia--documentation :around #'dotemacs-context-menu//inhibit-wrapper)
      (advice-add 'marginalia-annotate-command :around #'dotemacs-context-menu//inhibit-wrapper))))

(defun dotemacs-context-menu//setup-context-menu-override ()
  "Replace `context-menu-map' with inhibition-aware version."
  (unless dotemacs-context-menu--original-context-menu-map
    (setq dotemacs-context-menu--original-context-menu-map
          (symbol-function 'context-menu-map)))
  (fset 'context-menu-map #'dotemacs-context-menu//context-menu-map-replacement))

(defun dotemacs-context-menu//remove-help-advice ()
  "Remove advice from help functions."
  (advice-remove 'describe-key #'dotemacs-context-menu//inhibit-wrapper)
  (advice-remove 'describe-function #'dotemacs-context-menu//inhibit-wrapper)
  (advice-remove 'describe-variable #'dotemacs-context-menu//inhibit-wrapper))

(defun dotemacs-context-menu//remove-marginalia-advice ()
  "Remove advice from Marginalia functions."
  (when (featurep 'marginalia)
    (advice-remove 'marginalia--documentation #'dotemacs-context-menu//inhibit-wrapper)
    (advice-remove 'marginalia-annotate-command #'dotemacs-context-menu//inhibit-wrapper)))

(defun dotemacs-context-menu//restore-context-menu-override ()
  "Restore original `context-menu-map' function."
  (when dotemacs-context-menu--original-context-menu-map
    (fset 'context-menu-map dotemacs-context-menu--original-context-menu-map)
    (setq dotemacs-context-menu--original-context-menu-map nil)))

;;;###autoload
(defun dotemacs-context-menu/setup ()
  "Apply context menu integration for completion frameworks."
  (interactive)
  (dotemacs-context-menu//setup-context-menu-override)
  (dotemacs-context-menu//setup-help-advice)
  (dotemacs-context-menu//setup-marginalia-advice))

;;;###autoload
(defun dotemacs-context-menu/teardown ()
  "Remove context menu integration and restore defaults."
  (interactive)
  (dotemacs-context-menu//remove-help-advice)
  (dotemacs-context-menu//remove-marginalia-advice)
  (dotemacs-context-menu//restore-context-menu-override))

;;;###autoload
(defun dotemacs-context-menu/status ()
  "Display context menu integration status."
  (interactive)
  (let ((context-override (not (eq (symbol-function 'context-menu-map)
                                   dotemacs-context-menu--original-context-menu-map)))
        (help-advice (advice-member-p #'dotemacs-context-menu//inhibit-wrapper 'describe-key))
        (marginalia-advice (and (featurep 'marginalia)
                                (advice-member-p #'dotemacs-context-menu//inhibit-wrapper
                                                'marginalia--documentation))))))

;;;

(dotemacs-context-menu/setup)

(provide 'dotemacs-context-menu)

;;; dotemacs-context-menu.el ends here
