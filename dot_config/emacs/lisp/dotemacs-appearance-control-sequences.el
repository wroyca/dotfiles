;;; dotemacs-appearance-control-sequences.el --- Terminal control sequence integration -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(defgroup dotemacs-appearance-control-sequences nil
  "Terminal control sequence integration for Emacs."
  :group 'terminals
  :prefix "dotemacs-appearance-control-sequences-")

(defcustom dotemacs-appearance-control-sequences-sync-background t
  "Whether to synchronize terminal background with Emacs background."
  :type 'boolean
  :group 'dotemacs-appearance-control-sequences)

(defcustom dotemacs-appearance-control-sequences-query-timeout 0.5
  "Timeout in seconds when querying terminal capabilities."
  :type 'number
  :group 'dotemacs-appearance-control-sequences)

(defvar dotemacs-appearance-control-sequences--original-background nil
  "Original terminal background color before modification.")

(defvar dotemacs-appearance-control-sequences--supports-reset nil
  "Whether terminal supports the background reset control sequence (111).")

(defvar dotemacs-appearance-control-sequences--initialized nil
  "Whether terminal capability detection has been performed.")

;;; Internal functions

(defun dotemacs-appearance-control-sequences--query-terminal-color ()
  "Query the terminal for its current background color.
Returns the color string if successful, nil otherwise."
  (when (and (not (display-graphic-p))
             (>= emacs-major-version 25)) ; read-event with timeout needs Emacs 25+
    (let (chr response)
      ;; Send ANSI query sequence to request background color
      (send-string-to-terminal "\e]11;?\e\\")
         
      ;; Validation: properly formatted response begins with escape-bracket
      (when (and (eq (read-event nil nil dotemacs-appearance-control-sequences-query-timeout) ?\e)
                 (eq (read-event nil nil dotemacs-appearance-control-sequences-query-timeout) ?\]))
        
        ;; Collect response characters until terminating backslash
        (setq response "")
        (while (and (not (eq (setq chr (read-event nil nil dotemacs-appearance-control-sequences-query-timeout)) ?\\))
                    chr)
          (setq response (concat response (string chr))))
        
        ;; Extract RGB components from response (format: 11;rgb:RRRR/GGGG/BBBB)
        (when (string-match "11;rgb:\\([a-f0-9]+\\)/\\([a-f0-9]+\\)/\\([a-f0-9]+\\)" response)
          response)))))

(defun dotemacs-appearance-control-sequences--detect-terminal-capabilities ()
  "Detect terminal capabilities for background color control.
Returns non-nil if terminal supports background color setting."
  (when (and (not (display-graphic-p))
             (not dotemacs-appearance-control-sequences--initialized))
    ;; First establish baseline by capturing current background
    (setq dotemacs-appearance-control-sequences--original-background 
          (dotemacs-appearance-control-sequences--query-terminal-color))
    
    ;; Probe for reset capability - modern terminals support this sequence
    (when dotemacs-appearance-control-sequences--original-background
      (send-string-to-terminal "\e]111;?\e\\")
      (setq dotemacs-appearance-control-sequences--supports-reset
            (and (eq (read-event nil nil dotemacs-appearance-control-sequences-query-timeout) ?\e)
                 (eq (read-event nil nil dotemacs-appearance-control-sequences-query-timeout) ?\]))))
    
    (setq dotemacs-appearance-control-sequences--initialized t)
    dotemacs-appearance-control-sequences--original-background))

(defun dotemacs-appearance-control-sequences--update-terminal-background (&optional frame)
  "Update terminal background to match FRAME's background color.
If FRAME is nil, use the current frame."
  (when (and dotemacs-appearance-control-sequences-sync-background 
             (not (display-graphic-p))
             dotemacs-appearance-control-sequences--initialized)
    (let ((bg (frame-parameter frame 'background-color)))
      (when bg
        (send-string-to-terminal
         (format "\e]11;%s\a" bg))))))

(defun dotemacs-appearance-control-sequences--reset-terminal-background (&rest _args)
  "Reset terminal background to its original state.
This can be called with any number of arguments, allowing it to be
used with various hooks."
  (when (and dotemacs-appearance-control-sequences-sync-background 
             (not (display-graphic-p))
             dotemacs-appearance-control-sequences--initialized)
    (if dotemacs-appearance-control-sequences--supports-reset
        ;; Use the dedicated reset sequence if supported
        (send-string-to-terminal "\e]111;\a")
      ;; Otherwise, restore the original background color if we saved it
      (when dotemacs-appearance-control-sequences--original-background
        (send-string-to-terminal
         (format "\e]11;%s\a" 
                 (progn
                   (string-match "11;\\(.*\\)" dotemacs-appearance-control-sequences--original-background)
                   (match-string 1 dotemacs-appearance-control-sequences--original-background))))))))

(defun dotemacs-appearance-control-sequences--after-theme-change (&rest _)
  "Update terminal background after theme changes.
This ignores all arguments and just updates the terminal background.
It is designed to be used as advice for theme-related functions."
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
        ;; Perform one-time terminal capability discovery
        (unless dotemacs-appearance-control-sequences--initialized
          (dotemacs-appearance-control-sequences--detect-terminal-capabilities))
        
        ;; Register lifecycle events requiring background updates
        (add-hook 'after-make-frame-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
        (add-hook 'window-configuration-change-hook #'dotemacs-appearance-control-sequences--update-terminal-background)
        (add-hook 'kill-emacs-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
        (add-hook 'suspend-tty-functions #'dotemacs-appearance-control-sequences--reset-terminal-background)
        (add-hook 'resume-tty-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
        
        ;; Theme changes require special handling via advice
        (advice-add 'load-theme :after #'dotemacs-appearance-control-sequences--after-theme-change)
        
        ;; Apply immediately to sync current state
        (dotemacs-appearance-control-sequences--update-terminal-background))
    
    ;; Deregistration to ensure clean state
    (remove-hook 'after-make-frame-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
    (remove-hook 'window-configuration-change-hook #'dotemacs-appearance-control-sequences--update-terminal-background)
    (remove-hook 'kill-emacs-hook #'dotemacs-appearance-control-sequences--reset-terminal-background)
    (remove-hook 'suspend-tty-functions #'dotemacs-appearance-control-sequences--reset-terminal-background)
    (remove-hook 'resume-tty-functions #'dotemacs-appearance-control-sequences--update-terminal-background)
    
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