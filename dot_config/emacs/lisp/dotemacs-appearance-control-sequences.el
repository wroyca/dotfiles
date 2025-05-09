;;; dotemacs-appearance-control-sequences.el --- Terminal control sequence integration -*- lexical-binding: t -*-

;;; Commentary:
;; This module provides ANSI control sequences to synchronize Emacs
;; background color with the terminal emulator's background.
;;
;; It is designed for modern terminal emulators that support
;; xterm-compatible sequences OSC 11 and OSC 111, which set and reset
;; the background color respectively. Emacs emits the appropriate
;; sequence during lifecycle events such as frame creation, theme
;; changes, or Emacs shutdown.
;;
;; NOTE: this doesn't attempt to probe terminal capabilities at
;; runtime. Querying ANSI sequences such as OSC 11 or OSC 111 causes
;; the terminal to echo back a response. Since Emacs reads from the
;; same input stream, this echo can corrupt user input, break
;; interactive sessions, or cause subtle and hard-to-debug side
;; effects. Therefore, we assume the presence of OSC 11 and OSC 111 as
;; a baseline. If the terminal does not support these sequences, they
;; are safely ignored, resulting in a benign no-op rather than a
;; failure.
;;
;; In other words, the design philosophy is to favor silent
;; degradation over invasive probing.  Terminal incompatibility is not
;; considered a hard error.


;;; Code:

(defgroup dotemacs-appearance-control-sequences nil
  "Terminal control sequence integration for Emacs."
  :group 'terminals
  :prefix "dotemacs-appearance-control-sequences-")

(defcustom dotemacs-appearance-control-sequences-sync-background t
  "Whether to synchronize terminal background with Emacs background."
  :type 'boolean
  :group 'dotemacs-appearance-control-sequences)

(defvar dotemacs-appearance-control-sequences--last-sent-background nil
  "Last background color sent to the terminal.")

(defun dotemacs-appearance-control-sequences--update-terminal-background (&optional frame)
  "Update terminal background to match FRAME's background color.

If FRAME is nil, use the current frame."
  (when (and dotemacs-appearance-control-sequences-sync-background
          (not (display-graphic-p)))
    (let ((bg (frame-parameter frame 'background-color)))
      (send-string-to-terminal (format "\e]11;%s\a" bg)))))

(defun dotemacs-appearance-control-sequences--reset-terminal-background (&rest _args)
  "Reset terminal background to its default value.

This sends the ANSI OSC 111 sequence which, if supported, reverts the
background color to its default. If unsupported, the terminal ignores
it."
  (when (and dotemacs-appearance-control-sequences-sync-background
             (not (display-graphic-p)))
    (send-string-to-terminal "\e]111;\a")))

(defun dotemacs-appearance-control-sequences--after-theme-change (&rest _)
  "Update terminal background after theme changes.

This ignores all arguments and just updates the terminal background.  It
is designed to be used as advice for theme-related functions."
  (dotemacs-appearance-control-sequences--update-terminal-background))

;;;###autoload
(define-minor-mode dotemacs-appearance-control-sequences-mode
  "Toggle terminal control sequence integration mode.

When enabled, synchronizes terminal background with Emacs background
and ensures proper restoration on exit."
  :global t
  :lighter " CS"
  (if dotemacs-appearance-control-sequences-mode
      (progn
        ;; Register lifecycle events requiring background updates
        (add-hook 'after-make-frame-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
        (add-hook 'window-configuration-change-hook #'dotemacs-appearance-control-sequences--update-terminal-background)
        (add-hook 'kill-emacs-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
        (add-hook 'suspend-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
        (add-hook 'suspend-resume-hook #'dotemacs-appearance-control-sequences--update-terminal-background)

        ;; Theme changes require special handling via advice
        (advice-add 'load-theme :after #'dotemacs-appearance-control-sequences--after-theme-change)

        ;; Apply immediately to sync current state
        (dotemacs-appearance-control-sequences--update-terminal-background))

    ;; Deregister hooks and cleanup
    (remove-hook 'after-make-frame-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
    (remove-hook 'window-configuration-change-hook #'dotemacs-appearance-control-sequences--update-terminal-background)
    (remove-hook 'kill-emacs-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
    (remove-hook 'suspend-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
    (remove-hook 'suspend-resume-hook #'dotemacs-appearance-control-sequences--update-terminal-background)

    (advice-remove 'load-theme #'dotemacs-appearance-control-sequences--after-theme-change)

    ;; Explicit cleanup for deterministic behavior
    (dotemacs-appearance-control-sequences--reset-terminal-background)))

;;;###autoload
(defun dotemacs-appearance-control-sequences-update-now ()
  "Force an immediate update of the terminal background color."
  (interactive)
  (dotemacs-appearance-control-sequences--update-terminal-background))

;;;###autoload
(defun dotemacs-appearance-control-sequences-reset-now ()
  "Force an immediate reset of the terminal background color."
  (interactive)
  (dotemacs-appearance-control-sequences--reset-terminal-background))

(dotemacs-appearance-control-sequences-mode 1)

(provide 'dotemacs-appearance-control-sequences)

;;; dotemacs-appearance-control-sequences.el ends here
