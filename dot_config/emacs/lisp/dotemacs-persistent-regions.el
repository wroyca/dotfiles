;;; dotemacs-persistent-regions.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(require 'delsel)
(require 'mwheel)
(require 'cl-lib)

(defgroup dotemacs-persistent-regions nil
  "Modern text selection behavior for Emacs."
  :group 'editing
  :group 'convenience
  :prefix "dotemacs-persistent-regions-")

(defgroup dotemacs-persistent-regions-visual nil
  "Visual appearance and overlay configuration."
  :group 'dotemacs-persistent-regions
  :prefix "dotemacs-persistent-regions-")

(defgroup dotemacs-persistent-regions-behavior nil
  "Selection behavior and interaction policies."
  :group 'dotemacs-persistent-regions
  :prefix "dotemacs-persistent-regions-")

(defgroup dotemacs-persistent-regions-integration nil
  "Integration with other Emacs packages and systems."
  :group 'dotemacs-persistent-regions
  :prefix "dotemacs-persistent-regions-")

(defcustom dotemacs-persistent-regions-overlay-priority 1001
  "Priority value for dotemacs-persistent selection overlays.
Higher values cause overlays to appear above other overlays in the same
region.  The default value is set higher than most standard Emacs
overlays for selection visibility."
  :type 'integer
  :group 'dotemacs-persistent-regions-visual
  :safe 'integerp)

(defcustom dotemacs-persistent-regions-overlay-face 'region
  "Face used for displaying dotemacs-persistent selection overlays.
This face determines the visual appearance of dotemacs-persistent selections.
The default uses the standard `region' face align with normal
Emacs region highlighting."
  :type 'face
  :group 'dotemacs-persistent-regions-visual
  :safe 'facep)

(defcustom dotemacs-persistent-regions-preview-length 50
  "Maximum character length for selection preview in messages.
When displaying feedback about selection operations, text longer than
this value will be truncated with ellipsis to prevent minibuffer
overflow."
  :type 'integer
  :group 'dotemacs-persistent-regions-visual
  :safe 'integerp)

(defcustom dotemacs-persistent-regions-deactivate-on-movement t
  "When non-nil, moving point will clear any existing dotemacs-persistent selection.
This setting does not affect scrolling operations, which always preserve
selections regardless of this value."
  :type 'boolean
  :group 'dotemacs-persistent-regions-behavior
  :safe 'booleanp)

(defcustom dotemacs-persistent-regions-enable-mouse-support t
  "When non-nil, mouse selections are automatically converted to
persistent selections."
  :type 'boolean
  :group 'dotemacs-persistent-regions-integration
  :safe 'booleanp)

(defcustom dotemacs-persistent-regions-debug-mode nil
  "When non-nil, debug messages are sent to the *Messages*
buffer with a `PR-DEBUG:' prefix."
  :type 'boolean
  :group 'dotemacs-persistent-regions-integration
  :safe 'booleanp)

(defvar-local dotemacs-persistent-regions--selection-begin nil
  "Beginning position of the current dotemacs-persistent selection.
This variable holds the buffer position as an integer marking the
start of the current dotemacs-persistent selection.  This variable should not
be modified directly; use the provided API functions instead.")

(defvar-local dotemacs-persistent-regions--selection-end nil
  "Ending position of the current dotemacs-persistent selection.
This variable holds the buffer position as an integer marking the
end of the current dotemacs-persistent selection.  This variable should not
be modified directly; use the provided API functions instead.")

(defvar-local dotemacs-persistent-regions--overlays nil
  "List of overlays used for visual selection feedback.
This list contains all overlays currently managed by dotemacs-persistent-regions
in the current buffer.  Direct manipulation of this list is not
recommended; use the overlay management functions instead.")

(defvar-local dotemacs-persistent-regions--last-command-was-selection nil
  "Non-nil if the last command was a selection extension command.
This variable is used to implement intelligent selection behavior,
helping distinguish between intentional selection operations and
incidental cursor movement.")

(defvar dotemacs-persistent-regions--typing-commands
  '(self-insert-command
    org-self-insert-command)
  "List of commands that should trigger type-to-replace behavior.
These commands typically insert text and should replace any existing
persistent selection with the inserted content.")

(defvar dotemacs-persistent-regions--selection-commands
  '(dotemacs-persistent-regions-select-forward-word
    dotemacs-persistent-regions-select-backward-word
    dotemacs-persistent-regions-select-forward-paragraph
    dotemacs-persistent-regions-select-backward-paragraph
    dotemacs-persistent-regions-select-line
    dotemacs-persistent-regions-select-sexp
    dotemacs-persistent-regions-select-symbol)
  "List of commands that extend dotemacs-persistent selections.
These commands are recognized as intentional selection extension
operations and trigger appropriate feedback and state management.")

(defvar dotemacs-persistent-regions--movement-commands
  '(forward-char
    backward-char
    next-line
    previous-line
    forward-word
    backward-word
    forward-paragraph
    backward-paragraph
    beginning-of-line
    end-of-line
    beginning-of-buffer
    end-of-buffer
    goto-line
    goto-char
    move-to-column)
  "List of commands that move point without extending selections.
These commands represent cursor movement operations that should clear
persistent selections when `dotemacs-persistent-regions-deactivate-on-movement'
is non-nil.  Scrolling commands are intentionally excluded since
scrolling should always preserve selections.")

(defvar dotemacs-persistent-regions--scroll-commands
  '(scroll-up
    scroll-down
    scroll-up-command
    scroll-down-command
    recenter
    recenter-top-bottom
    mwheel-scroll
    pixel-scroll-precision
    dotemacs-persistent-regions--scroll-preserving-selection)
  "List of commands that scroll the buffer without affecting selections.
These commands represent scrolling operations that should never clear
persistent selections, regardless of the value of
`dotemacs-persistent-regions-deactivate-on-movement'.")

(defvar dotemacs-persistent-regions--mouse-drag-commands
  '(mouse-drag-region
    mouse-set-region
    mouse-set-point
    mouse-start-rectangle
    mouse-save-then-kill)
  "List of mouse commands that initiate drag operations.
These commands are monitored for state transitions when
starting new mouse-based selections.")

(defvar dotemacs-persistent-regions--mouse-bindings-active nil
  "Non-nil if global mouse bindings are currently active.")

(defun dotemacs-persistent-regions--debug-log (format-string &rest args)
  "Log debug message if debug mode is enabled.
FORMAT-STRING and ARGS are passed to `format' to create the log message.
Debug messages are prefixed with `PR-DEBUG:' and sent to the *Messages*
buffer when `dotemacs-persistent-regions-debug-mode' is non-nil."
  (when dotemacs-persistent-regions-debug-mode
    (apply #'message (concat "PR-DEBUG: " format-string) args)))

(defun dotemacs-persistent-regions--format-preview (begin end)
  "Format a preview string for the region between BEGIN and END.
The preview is truncated to `dotemacs-persistent-regions-preview-length'
characters to prevent minibuffer overflow.  Multi-line content is
converted to a single line with spaces for readability."
  (when (and begin end (< begin end))
    (let* ((text (buffer-substring-no-properties begin end))
           (single-line (replace-regexp-in-string "[\n\r\t]+" " " text))
           (length (length single-line))
           (preview (if (> length dotemacs-persistent-regions-preview-length)
                        (substring single-line 0 dotemacs-persistent-regions-preview-length)
                      single-line)))
      (if (> length dotemacs-persistent-regions-preview-length)
          (concat preview "...")
        preview))))

(defun dotemacs-persistent-regions--create-overlay (begin end)
  "Create a dotemacs-persistent selection overlay between BEGIN and END positions.
The overlay uses the face and priority specified in the customization
variables and is marked with the `dotemacs-persistent-regions' property for
identification.  Return the newly created overlay object."
  (when (and (>= begin (point-min))
             (<= end (point-max))
             (< begin end))
    (let ((overlay (make-overlay begin end nil t)))
      (overlay-put overlay 'priority dotemacs-persistent-regions-overlay-priority)
      (overlay-put overlay 'face dotemacs-persistent-regions-overlay-face)
      (overlay-put overlay 'dotemacs-persistent-regions t)
      (overlay-put overlay 'evaporate t)
      (push overlay dotemacs-persistent-regions--overlays)
      (dotemacs-persistent-regions--debug-log "Created overlay from %d to %d" begin end)
      overlay)))

(defun dotemacs-persistent-regions--clear-overlays ()
  "Remove all dotemacs-persistent selection overlays in the current buffer.
This function removes overlays both from the tracked list and from the
buffer itself for cleanup.  It is safe to call this function multiple
times or when no overlays exist."
  (dolist (overlay dotemacs-persistent-regions--overlays)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq dotemacs-persistent-regions--overlays nil)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'dotemacs-persistent-regions)
      (delete-overlay overlay)))
  (dotemacs-persistent-regions--debug-log "Cleared all overlays"))

(defun dotemacs-persistent-regions--normalize-selection (begin end)
  "Normalize selection boundaries BEGIN and END.
Return a cons cell (BEG . END) where BEG is always less than or equal
to END, regardless of the direction of selection creation.  The
normalized positions are also constrained to valid buffer positions."
  (let ((norm-begin (max (point-min) (min begin end)))
        (norm-end (min (point-max) (max begin end))))
    (cons norm-begin norm-end)))

(defun dotemacs-persistent-regions--set-selection (begin end)
  "Create a dotemacs-persistent selection between BEGIN and END positions.
This function sets up both the internal state variables and the visual
overlay representation of the selection.  The selection boundaries are
automatically normalized."
  (condition-case err
      (let ((normalized (dotemacs-persistent-regions--normalize-selection begin end)))
        (setq dotemacs-persistent-regions--selection-begin (car normalized)
              dotemacs-persistent-regions--selection-end (cdr normalized))
        (dotemacs-persistent-regions--clear-overlays)
        (dotemacs-persistent-regions--create-overlay
         dotemacs-persistent-regions--selection-begin
         dotemacs-persistent-regions--selection-end)
        (dotemacs-persistent-regions--debug-log "Set selection from %d to %d"
                                       dotemacs-persistent-regions--selection-begin
                                       dotemacs-persistent-regions--selection-end)
        (run-hook-with-args 'dotemacs-persistent-regions-selection-created-hook
                            dotemacs-persistent-regions--selection-begin
                            dotemacs-persistent-regions--selection-end))
    (error
     (dotemacs-persistent-regions--debug-log "Error setting selection: %s" err)
     (dotemacs-persistent-regions--clear-selection))))

(defun dotemacs-persistent-regions--clear-selection ()
  "Clear the current dotemacs-persistent selection.
Remove both the visual overlay representation and the internal state
variables.  This function is idempotent and safe to call even when no
selection exists."
  (when (dotemacs-persistent-regions--has-selection-p)
    (dotemacs-persistent-regions--debug-log "Clearing selection from %d to %d"
                                   dotemacs-persistent-regions--selection-begin
                                   dotemacs-persistent-regions--selection-end)
    (run-hook-with-args 'dotemacs-persistent-regions-selection-cleared-hook
                        dotemacs-persistent-regions--selection-begin
                        dotemacs-persistent-regions--selection-end))
  (dotemacs-persistent-regions--clear-overlays)
  (setq dotemacs-persistent-regions--selection-begin nil
        dotemacs-persistent-regions--selection-end nil)
  (dotemacs-persistent-regions--debug-log "Selection cleared"))

(defun dotemacs-persistent-regions--has-selection-p ()
  "Return non-nil if a dotemacs-persistent selection currently exists.
This function checks both that selection coordinates are set and that
they represent a valid, non-empty selection within the current buffer
boundaries."
  (and dotemacs-persistent-regions--selection-begin
       dotemacs-persistent-regions--selection-end
       (< dotemacs-persistent-regions--selection-begin dotemacs-persistent-regions--selection-end)
       (>= dotemacs-persistent-regions--selection-begin (point-min))
       (<= dotemacs-persistent-regions--selection-end (point-max))))

(defun dotemacs-persistent-regions--extend-selection (move-function)
  "Extend the dotemacs-persistent selection using MOVE-FUNCTION.
This function implements the core logic for extending existing selections
or creating new ones based on the current state.  MOVE-FUNCTION should
be a function that moves point, such as `forward-word' or `forward-paragraph'."
  (setq dotemacs-persistent-regions--last-command-was-selection t)
  (condition-case err
      (cond
       ((and (region-active-p)
             (not (dotemacs-persistent-regions--has-selection-p)))
        (let ((begin (region-beginning))
              (end (region-end)))
          (dotemacs-persistent-regions--set-selection begin end)
          (deactivate-mark t)
          (goto-char end)
          (funcall move-function)
          (dotemacs-persistent-regions--update-selection-end (point))))
       ((dotemacs-persistent-regions--has-selection-p)
        (let ((old-point (point))
              (selection-begin dotemacs-persistent-regions--selection-begin)
              (selection-end dotemacs-persistent-regions--selection-end))
          (funcall move-function)
          (let ((new-point (point)))
            (cond
             ((= old-point selection-end)
              (dotemacs-persistent-regions--set-selection selection-begin new-point))
             ((= old-point selection-begin)
              (dotemacs-persistent-regions--set-selection new-point selection-end))
             (t
              (if (< new-point old-point)
                  (dotemacs-persistent-regions--set-selection new-point selection-end)
                (dotemacs-persistent-regions--set-selection selection-begin new-point)))))))
       (t
        (let ((start-pos (point)))
          (funcall move-function)
          (dotemacs-persistent-regions--set-selection start-pos (point)))))
    (error
     (dotemacs-persistent-regions--debug-log "Error extending selection: %s" err)
     (message "Error extending selection: %s" err))))

(defun dotemacs-persistent-regions--update-selection-end (new-end)
  "Update the dotemacs-persistent selection to end at NEW-END position.
Handle direction changes automatically by normalizing the selection
boundaries and updating the visual representation accordingly."
  (when (dotemacs-persistent-regions--has-selection-p)
    (dotemacs-persistent-regions--set-selection dotemacs-persistent-regions--selection-begin
                                       new-end)))

(defun dotemacs-persistent-regions--move-point (move-function)
  "Move point using MOVE-FUNCTION with intelligent selection handling.
This function provides the core behavior for navigation that may or may
not preserve existing selections, depending on the current configuration
and command context."
  (setq dotemacs-persistent-regions--last-command-was-selection nil)
  (condition-case err
      (progn
        (dotemacs-persistent-regions--debug-log "Move point called, command: %s, deactivate-on-movement: %s, has-selection: %s"
                                       this-command
                                       dotemacs-persistent-regions-deactivate-on-movement
                                       (dotemacs-persistent-regions--has-selection-p))
        (when (and dotemacs-persistent-regions-deactivate-on-movement
                   (dotemacs-persistent-regions--has-selection-p)
                   (not (memq this-command dotemacs-persistent-regions--scroll-commands)))
          (dotemacs-persistent-regions--debug-log "Clearing selection due to movement command: %s" this-command)
          (dotemacs-persistent-regions--clear-selection))
        (funcall move-function))
    (error
     (dotemacs-persistent-regions--debug-log "Error moving point: %s" err)
     (message "Error moving point: %s" err))))

(defun dotemacs-persistent-regions--replace-selection-advice (orig-fun &rest args)
  "Advice function to implement type-to-replace behavior.
This function wraps text insertion commands to implement automatic
replacement of dotemacs-persistent selections.  When a dotemacs-persistent selection exists,
it is replaced with the new content.

ORIG-FUN is the original command function.
ARGS are the arguments passed to the original command."
  (if (dotemacs-persistent-regions--has-selection-p)
      (let ((begin dotemacs-persistent-regions--selection-begin)
            (end dotemacs-persistent-regions--selection-end))
        (dotemacs-persistent-regions--clear-selection)
        (delete-region begin end)
        (goto-char begin)
        (apply orig-fun args)
        (run-hook-with-args 'dotemacs-persistent-regions-selection-replaced-hook
                            begin end))
    (apply orig-fun args)))

;;;###autoload
(defun dotemacs-persistent-regions-select-forward-word ()
  "Extend dotemacs-persistent selection forward by one word.
If no selection exists, create a new selection from the current point
to the end of the next word.  If a selection already exists, extend it
forward by one word."
  (interactive)
  (dotemacs-persistent-regions--extend-selection #'forward-word))

;;;###autoload
(defun dotemacs-persistent-regions-select-backward-word ()
  "Extend dotemacs-persistent selection backward by one word.
If no selection exists, create a new selection from the current point
to the beginning of the previous word.  If a selection already exists,
extend it backward by one word."
  (interactive)
  (dotemacs-persistent-regions--extend-selection #'backward-word))

;;;###autoload
(defun dotemacs-persistent-regions-select-forward-paragraph ()
  "Extend dotemacs-persistent selection forward by one paragraph.
If no selection exists, create a new selection from the current point
to the end of the next paragraph.  If a selection already exists,
extend it forward by one paragraph."
  (interactive)
  (dotemacs-persistent-regions--extend-selection #'forward-paragraph))

;;;###autoload
(defun dotemacs-persistent-regions-select-backward-paragraph ()
  "Extend dotemacs-persistent selection backward by one paragraph.
If no selection exists, create a new selection from the current point
to the beginning of the previous paragraph.  If a selection already exists,
extend it backward by one paragraph."
  (interactive)
  (dotemacs-persistent-regions--extend-selection #'backward-paragraph))

;;;###autoload
(defun dotemacs-persistent-regions-select-line ()
  "Create or extend dotemacs-persistent selection to include the current line.
If no selection exists, select the entire current line including the
newline character.  If a selection already exists, extend it to include
additional complete lines based on the cursor position."
  (interactive)
  (if (dotemacs-persistent-regions--has-selection-p)
      (dotemacs-persistent-regions--extend-selection
       (lambda () (end-of-line) (forward-char 1)))
    (let ((start (line-beginning-position))
          (end (line-beginning-position 2)))
      (dotemacs-persistent-regions--set-selection start end))))

;;;###autoload
(defun dotemacs-persistent-regions-select-line-above ()
  "Extend dotemacs-persistent selection to include the line above.
If no selection exists, create a new selection from the current line
to the line above.  If a selection already exists, extend it to include
the line above."
  (interactive)
  (dotemacs-persistent-regions--extend-selection
   (lambda ()
     (forward-line -1)
     (beginning-of-line))))

;;;###autoload
(defun dotemacs-persistent-regions-select-line-below ()
  "Extend dotemacs-persistent selection to include the line below.
If no selection exists, create a new selection from the current line
to the line below.  If a selection already exists, extend it to include
the line below."
  (interactive)
  (dotemacs-persistent-regions--extend-selection
   (lambda ()
     (forward-line 1)
     (end-of-line))))

;;;###autoload
(defun dotemacs-persistent-regions-select-sexp ()
  "Extend dotemacs-persistent selection by one s-expression.
This command is particularly useful for selecting Lisp code structures.
If no selection exists, select the s-expression at or after point.
If a selection already exists, extend it to include additional
s-expressions."
  (interactive)
  (dotemacs-persistent-regions--extend-selection #'forward-sexp))

;;;###autoload
(defun dotemacs-persistent-regions-select-symbol ()
  "Extend dotemacs-persistent selection by one symbol.
If no selection exists, select the symbol at point.  If a selection
already exists, extend it to include additional symbols."
  (interactive)
  (dotemacs-persistent-regions--extend-selection
   (lambda ()
     (skip-syntax-forward "_w")
     (unless (eobp) (forward-char)))))

;;;###autoload
(defun dotemacs-persistent-regions-forward-char ()
  "Move forward one character with intelligent selection handling.
If `dotemacs-persistent-regions-deactivate-on-movement' is non-nil, clear any
existing dotemacs-persistent selection.  Otherwise, preserve the selection
while moving the cursor."
  (interactive)
  (dotemacs-persistent-regions--move-point #'forward-char))

;;;###autoload
(defun dotemacs-persistent-regions-backward-char ()
  "Move backward one character with selection.
If `dotemacs-persistent-regions-deactivate-on-movement' is non-nil, clear any
existing dotemacs-persistent selection.  Otherwise, preserve the selection
while moving the cursor."
  (interactive)
  (dotemacs-persistent-regions--move-point #'backward-char))

;;;###autoload
(defun dotemacs-persistent-regions-next-line ()
  "Move to next line with selection.
If `dotemacs-persistent-regions-deactivate-on-movement' is non-nil, clear any
existing dotemacs-persistent selection.  Otherwise, preserve the selection
while moving the cursor."
  (interactive)
  (dotemacs-persistent-regions--move-point #'next-line))

;;;###autoload
(defun dotemacs-persistent-regions-previous-line ()
  "Move to previous line with selection.
If `dotemacs-persistent-regions-deactivate-on-movement' is non-nil, clear any
existing dotemacs-persistent selection.  Otherwise, preserve the selection
while moving the cursor."
  (interactive)
  (dotemacs-persistent-regions--move-point #'previous-line))

;;;###autoload
(defun dotemacs-persistent-regions-copy ()
  "Copy the dotemacs-persistent selection to the kill ring.
If a dotemacs-persistent selection exists, copy its contents to the kill ring
and provide user feedback.  If no dotemacs-persistent selection exists but a
standard Emacs region is active, copy that instead.  After copying,
the dotemacs-persistent selection is cleared."
  (interactive)
  (cond
   ((dotemacs-persistent-regions--has-selection-p)
    (copy-region-as-kill dotemacs-persistent-regions--selection-begin
                         dotemacs-persistent-regions--selection-end)
    (message "Copied: %s"
             (dotemacs-persistent-regions--format-preview
              dotemacs-persistent-regions--selection-begin
              dotemacs-persistent-regions--selection-end))
    (dotemacs-persistent-regions--clear-selection))
   ((region-active-p)
    (copy-region-as-kill (region-beginning) (region-end))
    (message "Copied region")
    (deactivate-mark))
   (t
    (message "No selection to copy"))))

;;;###autoload
(defun dotemacs-persistent-regions-kill ()
  "Kill the dotemacs-persistent selection to the kill ring.
Similar to `dotemacs-persistent-regions-copy' but removes the selected text
from the buffer in addition to copying it to the kill ring."
  (interactive)
  (cond
   ((dotemacs-persistent-regions--has-selection-p)
    (kill-region dotemacs-persistent-regions--selection-begin
                 dotemacs-persistent-regions--selection-end)
    (message "Killed dotemacs-persistent selection")
    (dotemacs-persistent-regions--clear-selection))
   ((region-active-p)
    (kill-region (region-beginning) (region-end))
    (message "Killed region"))
   (t
    (message "No selection to kill"))))

;;;###autoload
(defun dotemacs-persistent-regions-clear ()
  "Clear the current dotemacs-persistent selection without copying.
Remove the dotemacs-persistent selection and its visual representation without
affecting the kill ring or clipboard."
  (interactive)
  (if (dotemacs-persistent-regions--has-selection-p)
      (progn
        (dotemacs-persistent-regions--clear-selection)
        (message "Cleared dotemacs-persistent selection"))
    (message "No dotemacs-persistent selection to clear")))

;;;###autoload
(defun dotemacs-persistent-regions-mark-whole-buffer ()
  "Create a dotemacs-persistent selection encompassing the entire buffer.
This command selects all text in the current buffer and converts it
to a dotemacs-persistent selection."
  (interactive)
  (dotemacs-persistent-regions--set-selection (point-min) (point-max))
  (message "Selected entire buffer"))

(defun dotemacs-persistent-regions--on-region-active ()
  "Handle region activation events, typically from mouse selection.
This function converts active regions to dotemacs-persistent selections when
appropriate."
  (dotemacs-persistent-regions--debug-log "Region active hook called, region active: %s, point: %d, mark: %d"
                                 (region-active-p)
                                 (point)
                                 (if mark-active (mark) -1))
  (when (and (region-active-p)
             dotemacs-persistent-regions-enable-mouse-support
             (not (dotemacs-persistent-regions--has-selection-p)))
    (let ((begin (region-beginning))
          (end (region-end)))
      (dotemacs-persistent-regions--debug-log "Converting region (%d to %d) to dotemacs-persistent selection" begin end)
      (when (< begin end)  ; Only convert non-empty regions
        (dotemacs-persistent-regions--set-selection begin end)
        (deactivate-mark t)
        (dotemacs-persistent-regions--debug-log "Successfully converted region to dotemacs-persistent selection")))))

(defun dotemacs-persistent-regions--clear-on-mouse-start ()
  "Clear dotemacs-persistent selections when starting new mouse operations."
  (when (and (memq this-command dotemacs-persistent-regions--mouse-drag-commands)
             (dotemacs-persistent-regions--has-selection-p))
    (dotemacs-persistent-regions--clear-selection)))

(defun dotemacs-persistent-regions--post-command-check ()
  "Check for region activation after command completion.
This function is called from post-command-hook to detect when mouse
drag operations have completed and left an active region that should
be converted to a dotemacs-persistent selection."
  (when (and (region-active-p)
             dotemacs-persistent-regions-enable-mouse-support
             (not (dotemacs-persistent-regions--has-selection-p))
             (memq this-command dotemacs-persistent-regions--mouse-drag-commands))
    (let ((begin (region-beginning))
          (end (region-end)))
      (dotemacs-persistent-regions--debug-log "Post-command: Converting region (%d to %d) from command %s" begin end this-command)
      (when (< begin end)  ; Only convert non-empty regions
        (dotemacs-persistent-regions--set-selection begin end)
        (deactivate-mark t)
        (dotemacs-persistent-regions--debug-log "Post-command: Successfully converted region to dotemacs-persistent selection")))))

(defun dotemacs-persistent-regions--handle-mouse-drag (event)
  "Handle mouse drag events with dotemacs-persistent selection integration.
EVENT is the mouse event."
  (interactive "e")
  (dotemacs-persistent-regions--debug-log "Mouse drag handler called, mode active: %s, mouse support: %s"
                                 dotemacs-persistent-regions-mode
                                 dotemacs-persistent-regions-enable-mouse-support)
  (when (or dotemacs-persistent-regions--selection-begin
            dotemacs-persistent-regions--selection-end
            dotemacs-persistent-regions--overlays)
    (dotemacs-persistent-regions--clear-selection))
  (mouse-drag-region event)
  (dotemacs-persistent-regions--debug-log "After mouse drag, region active: %s" (region-active-p)))

(defun dotemacs-persistent-regions--scroll-preserving-selection (event)
  "Scroll while preserving dotemacs-persistent selections.
EVENT is the mouse wheel event."
  (interactive (list last-input-event))
  (let* ((selected-window (selected-window))
         (event-window (mwheel-event-window event))
         (current-window (if mouse-wheel-follow-mouse
                             (prog1 selected-window
                               (when (window-live-p event-window)
                                 (select-window event-window)))
                           selected-window))
         (buffer (window-buffer current-window))
         (original-point nil)
         (scroll-amount nil))
    (with-current-buffer buffer
      (when (region-active-p)
        (setq original-point (point))
        (setq dotemacs-persistent-regions--selection-begin (region-beginning)
              dotemacs-persistent-regions--selection-end (region-end))))
    (let* ((mods (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
           (amt-assoc (assoc mods mouse-wheel-scroll-amount)))
      (if amt-assoc
          (setq scroll-amount (cdr amt-assoc))
        (let ((amt-list mouse-wheel-scroll-amount))
          (while (consp (setq scroll-amount (pop amt-list))))))
      (when (floatp scroll-amount)
        (setq scroll-amount (1+ (truncate (* scroll-amount (window-height))))))
      (when (and mouse-wheel-progressive-speed (numberp scroll-amount))
        (setq scroll-amount (* scroll-amount (event-click-count event)))))
    (unwind-protect
        (let ((button (mwheel-event-button event)))
          (cond
           ((eq button mouse-wheel-down-event)
            (condition-case nil
                (funcall mwheel-scroll-down-function scroll-amount)
              (beginning-of-buffer
               (funcall mwheel-scroll-down-function)
               (set-window-start current-window (point-min)))))
           ((eq button mouse-wheel-up-event)
            (condition-case nil
                (funcall mwheel-scroll-up-function scroll-amount)
              (end-of-buffer
               (while t (funcall mwheel-scroll-up-function)))))
           ((or (eq button 'wheel-right) (eq button mouse-wheel-right-event))
            (when (boundp 'mwheel-scroll-right-function)
              (funcall mwheel-scroll-right-function scroll-amount)))
           ((or (eq button 'wheel-left) (eq button mouse-wheel-left-event))
            (when (boundp 'mwheel-scroll-left-function)
              (funcall mwheel-scroll-left-function scroll-amount)))
           (t
            (condition-case nil
                (funcall mwheel-scroll-down-function scroll-amount)
              (error nil)))))
      (when (and mouse-wheel-follow-mouse
                 (not (eq current-window selected-window))
                 (window-live-p selected-window))
        (select-window selected-window)))
    (with-current-buffer buffer
      (when (and original-point
                 dotemacs-persistent-regions--selection-begin
                 dotemacs-persistent-regions--selection-end
                 (/= (point) original-point))
        (dotemacs-persistent-regions--clear-overlays)
        (dotemacs-persistent-regions--create-overlay dotemacs-persistent-regions--selection-begin dotemacs-persistent-regions--selection-end)
        (deactivate-mark t)))
    (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
      (if mwheel-inhibit-click-event-timer
          (cancel-timer mwheel-inhibit-click-event-timer)
        (add-hook 'pre-command-hook 'mwheel-filter-click-events))
      (setq mwheel-inhibit-click-event-timer
            (run-with-timer mouse-wheel-inhibit-click-time nil
                            'mwheel-inhibit-click-timeout)))))

(defun dotemacs-persistent-regions--setup-keybindings ()
  "Set up key bindings for dotemacs-persistent regions mode.
All bindings are set up as buffer-local to avoid conflicts with global key bindings."
  (local-set-key (kbd "C-S-<right>") #'dotemacs-persistent-regions-select-forward-word)
  (local-set-key (kbd "C-S-<left>") #'dotemacs-persistent-regions-select-backward-word)
  (local-set-key (kbd "C-S-<down>") #'dotemacs-persistent-regions-select-line-below)
  (local-set-key (kbd "C-S-<up>") #'dotemacs-persistent-regions-select-line-above)
  (local-set-key (kbd "<right>") #'dotemacs-persistent-regions-forward-char)
  (local-set-key (kbd "<left>") #'dotemacs-persistent-regions-backward-char)
  (local-set-key (kbd "<down>") #'dotemacs-persistent-regions-next-line)
  (local-set-key (kbd "<up>") #'dotemacs-persistent-regions-previous-line)
  (local-set-key (kbd "C-<right>") (lambda () (interactive) (dotemacs-persistent-regions--move-point #'forward-word)))
  (local-set-key (kbd "C-<left>") (lambda () (interactive) (dotemacs-persistent-regions--move-point #'backward-word)))
  (local-set-key (kbd "C-<down>") (lambda () (interactive) (dotemacs-persistent-regions--move-point #'forward-paragraph)))
  (local-set-key (kbd "C-<up>") (lambda () (interactive) (dotemacs-persistent-regions--move-point #'backward-paragraph)))
  (local-set-key (kbd "C-c r c") #'dotemacs-persistent-regions-copy)
  (local-set-key (kbd "C-c r k") #'dotemacs-persistent-regions-kill)
  (local-set-key (kbd "C-c r x") #'dotemacs-persistent-regions-clear)
  (local-set-key (kbd "C-c r a") #'dotemacs-persistent-regions-mark-whole-buffer)
  (local-set-key (kbd "C-c r l") #'dotemacs-persistent-regions-select-line)
  (local-set-key (kbd "C-c r s") #'dotemacs-persistent-regions-select-sexp)
  (local-set-key (kbd "C-c r w") #'dotemacs-persistent-regions-select-symbol))

(defun dotemacs-persistent-regions--restore-keybindings ()
  "Restore original key bindings when disabling dotemacs-persistent regions mode."
  (local-unset-key (kbd "C-S-<right>"))
  (local-unset-key (kbd "C-S-<left>"))
  (local-unset-key (kbd "C-S-<down>"))
  (local-unset-key (kbd "C-S-<up>"))
  (local-unset-key (kbd "<right>"))
  (local-unset-key (kbd "<left>"))
  (local-unset-key (kbd "<down>"))
  (local-unset-key (kbd "<up>"))
  (local-unset-key (kbd "C-<right>"))
  (local-unset-key (kbd "C-<left>"))
  (local-unset-key (kbd "C-<down>"))
  (local-unset-key (kbd "C-<up>"))
  (local-unset-key (kbd "C-c r c"))
  (local-unset-key (kbd "C-c r k"))
  (local-unset-key (kbd "C-c r x"))
  (local-unset-key (kbd "C-c r a"))
  (local-unset-key (kbd "C-c r l"))
  (local-unset-key (kbd "C-c r s"))
  (local-unset-key (kbd "C-c r w")))

(defun dotemacs-persistent-regions--setup-advice ()
  "Set up function advice for type-to-replace behavior."
  (dolist (cmd dotemacs-persistent-regions--typing-commands)
    (when (fboundp cmd)
      (advice-add cmd :around #'dotemacs-persistent-regions--replace-selection-advice))))

(defun dotemacs-persistent-regions--remove-advice ()
  "Remove function advice when disabling dotemacs-persistent-regions."
  (dolist (cmd dotemacs-persistent-regions--typing-commands)
    (when (fboundp cmd)
      (advice-remove cmd #'dotemacs-persistent-regions--replace-selection-advice))))

(defun dotemacs-persistent-regions--setup-global-mouse-bindings ()
  "Set up global mouse bindings when the first buffer enables the mode.
Mouse events require global bindings to function properly across all
buffers."
  (unless dotemacs-persistent-regions--mouse-bindings-active
    (when dotemacs-persistent-regions-enable-mouse-support
      (global-set-key [(mouse-4)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [(mouse-5)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [(control mouse-4)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [(control mouse-5)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [(shift mouse-4)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [(shift mouse-5)] #'dotemacs-persistent-regions--scroll-preserving-selection)
      (global-set-key [down-mouse-1] #'dotemacs-persistent-regions--handle-mouse-drag)
      (setq dotemacs-persistent-regions--mouse-bindings-active t)
      (dotemacs-persistent-regions--debug-log "Installed global mouse bindings"))))

(defun dotemacs-persistent-regions--restore-global-mouse-bindings ()
  "Restore global mouse bindings when the last buffer disables the mode."
  (when dotemacs-persistent-regions--mouse-bindings-active
    (global-set-key [(mouse-4)] #'mwheel-scroll)
    (global-set-key [(mouse-5)] #'mwheel-scroll)
    (global-set-key [(control mouse-4)] #'mwheel-scroll)
    (global-set-key [(control mouse-5)] #'mwheel-scroll)
    (global-set-key [(shift mouse-4)] #'mwheel-scroll)
    (global-set-key [(shift mouse-5)] #'mwheel-scroll)
    (global-set-key [down-mouse-1] #'mouse-drag-region)
    (setq dotemacs-persistent-regions--mouse-bindings-active nil)
    (dotemacs-persistent-regions--debug-log "Restored global mouse bindings")))

(defun dotemacs-persistent-regions--setup-mouse-bindings ()
  "Set up mouse bindings for enhanced selection behavior."
  (dotemacs-persistent-regions--setup-global-mouse-bindings))

(defun dotemacs-persistent-regions--restore-mouse-bindings ()
  "Restore original mouse bindings when disabling mouse support."
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       dotemacs-persistent-regions-mode))
                   (buffer-list))
    (dotemacs-persistent-regions--restore-global-mouse-bindings)))

;;;###autoload
(define-minor-mode dotemacs-persistent-regions-mode
  "Toggle dotemacs-persistent regions mode in the current buffer."
  :lighter " PR3"
  :group 'dotemacs-persistent-regions
  :keymap (make-sparse-keymap)
  (if dotemacs-persistent-regions-mode
      (progn
        (dotemacs-persistent-regions--setup-keybindings)
        (dotemacs-persistent-regions--setup-mouse-bindings)
        (dotemacs-persistent-regions--setup-advice)
        (when dotemacs-persistent-regions-enable-mouse-support
          (add-hook 'activate-mark-hook #'dotemacs-persistent-regions--on-region-active nil t)
          (add-hook 'pre-command-hook #'dotemacs-persistent-regions--clear-on-mouse-start nil t)
          (add-hook 'post-command-hook #'dotemacs-persistent-regions--post-command-check nil t))
        (unless delete-selection-mode
          (delete-selection-mode 1))
        (dotemacs-persistent-regions--debug-log "Enabled dotemacs-persistent-regions-mode in buffer %s" (buffer-name))
        (dotemacs-persistent-regions--debug-log "Mouse bindings active: %s" dotemacs-persistent-regions--mouse-bindings-active))
    (dotemacs-persistent-regions--clear-selection)
    (dotemacs-persistent-regions--restore-keybindings)
    (dotemacs-persistent-regions--restore-mouse-bindings)
    (dotemacs-persistent-regions--remove-advice)
    (remove-hook 'activate-mark-hook #'dotemacs-persistent-regions--on-region-active t)
    (remove-hook 'pre-command-hook #'dotemacs-persistent-regions--clear-on-mouse-start t)
    (remove-hook 'post-command-hook #'dotemacs-persistent-regions--post-command-check t)
    (dotemacs-persistent-regions--debug-log "Disabled dotemacs-persistent-regions-mode in buffer %s" (buffer-name))))

;;;###autoload
(define-globalized-minor-mode dotemacs-persistent-regions-global-mode
  dotemacs-persistent-regions-mode
  (lambda ()
    (unless (or (minibufferp)
                (string-prefix-p " " (buffer-name))
                (memq major-mode '(fundamental-mode)))
      (dotemacs-persistent-regions-mode 1)))
  :group 'dotemacs-persistent-regions
  :require 'dotemacs-persistent-regions)

(defun dotemacs-persistent-regions--force-mode-activation ()
  "Force dotemacs-persistent-regions-mode to works around limitations in Emacs globalized minor mode."
  (condition-case err
      (when (and dotemacs-persistent-regions-global-mode
                 (not (minibufferp))
                 (not dotemacs-persistent-regions-mode)
                 (not (string-prefix-p " " (buffer-name)))
                 (not (memq major-mode '(fundamental-mode))))
        (dotemacs-persistent-regions-mode 1))
    (error
     (dotemacs-persistent-regions--debug-log "Error forcing mode activation: %s" err))))

(add-hook 'find-file-hook #'dotemacs-persistent-regions--force-mode-activation)
(add-hook 'buffer-list-update-hook #'dotemacs-persistent-regions--force-mode-activation)

(defvar dotemacs-persistent-regions-selection-created-hook nil
  "Hook run when a dotemacs-persistent selection is created.
Functions on this hook are called with two arguments: the beginning
and ending positions of the newly created selection.")

(defvar dotemacs-persistent-regions-selection-cleared-hook nil
  "Hook run when a dotemacs-persistent selection is cleared.
Functions on this hook are called with two arguments: the beginning
and ending positions of the selection that was cleared.")

(defvar dotemacs-persistent-regions-selection-replaced-hook nil
  "Hook run when a dotemacs-persistent selection is replaced with new content.
Functions on this hook are called with two arguments: the beginning
and ending positions of the original selection.")

;;;###autoload
(defun dotemacs-persistent-regions-current-selection-bounds ()
  "Return the bounds of the current dotemacs-persistent selection as (BEG . END).
Return nil if no dotemacs-persistent selection exists."
  (when (dotemacs-persistent-regions--has-selection-p)
    (cons dotemacs-persistent-regions--selection-begin
          dotemacs-persistent-regions--selection-end)))

;;;###autoload
(defun dotemacs-persistent-regions-current-selection-text ()
  "Return the text content of the current dotemacs-persistent selection.
Return nil if no dotemacs-persistent selection exists."
  (when (dotemacs-persistent-regions--has-selection-p)
    (buffer-substring-no-properties dotemacs-persistent-regions--selection-begin
                                    dotemacs-persistent-regions--selection-end)))

;;;###autoload
(defun dotemacs-persistent-regions-set-selection (begin end)
  "Programmatically set a dotemacs-persistent selection from BEGIN to END.
The selection boundaries are automatically normalized and validated."
  (dotemacs-persistent-regions--set-selection begin end))

;;;###autoload
(defun dotemacs-persistent-regions-has-selection-p ()
  "Return non-nil if a dotemacs-persistent selection currently exists."
  (dotemacs-persistent-regions--has-selection-p))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when dotemacs-persistent-regions-global-mode
              (dotemacs-persistent-regions-global-mode -1))))

(put 'dotemacs-persistent-regions--scroll-preserving-selection 'scroll-command t)

(dotemacs-persistent-regions-global-mode)

(provide 'dotemacs-persistent-regions)

;;; dotemacs-persistent-regions.el ends here
