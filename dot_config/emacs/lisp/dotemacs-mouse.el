;;; dotemacs-mouse.el --- Persistent selections -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Architectural Overview
;;
;; Overlay-Based Selection Model
;;
;; Represent selections using overlays instead of the point/mark pair:
;;
;; - Spatial independence: Decouple overlay visibility from
;;   `point'. Maintain visual selection continuity during cursor
;;   motion and scrolling.
;;
;; - Display priority enforcement: Assign overlays a priority of 1001
;;   to override conflicting visual properties from other packages.
;;
;; - Cached positional metadata: Store character positions in
;;   `dotemacs-mouse--region-begin' and `dotemacs-mouse--region-end'
;;   to eliminate overlay introspection at runtime.
;;
;; Input Event Interception
;;
;; Intercept interactive input events using the following strategies:
;;
;; - Function remapping: Remap mouse-related commands
;;   (e.g. `mwheel-scroll') to wrappers that preserve overlay
;;   selections during event execution.
;;
;; - Advice instrumentation: Advise mutating commands
;;   (e.g. `self-insert-command') to detect active overlay regions and
;;   enforce type-to-replace semantics.
;;
;; - Pre-command hook monitoring: Detect drag initiations and state
;;   transitions via `pre-command-hook'. Synchronize overlay state
;;   teardown and reinitialization.
;;
;; Selection State Management
;;
;; Track selection state using three coordinated components:
;;
;; 1. `dotemacs-mouse--region-begin' and `dotemacs-mouse--region-end':
;;    Store absolute buffer positions as integers. Avoid markers to
;;    prevent drift during unrelated edits.
;;
;; 2. `dotemacs-mouse--overlays': Maintain a list of active overlays
;;    representing the rendered selection.
;;
;; 3. `dotemacs-mouse/selection-mode': Indicate whether overlay-based
;;    selection is active in the current buffer.
;;
;; Compatibility Layer with Emacs Primitives
;;
;; Integrate with core Emacs subsystems:
;;
;; - `delete-selection-mode': Treat active overlays as virtual regions
;;   to support overwrite behavior.
;;
;; - `transient-mark-mode': Enable temporarily to delegate to built-in
;;   region logic when necessary.
;;
;; - Scroll subsystem (`mwheel.el'): Handle mouse wheel input in
;;   accordance with `scroll-command' conventions to maintain motion
;;   compatibility.
;;
;; Selection Model Transitions
;;
;; Use `dotemacs-mouse/activate-mark' to detect the current selection
;; state and perform transitions between overlay-based selections and
;; point/mark regions. Fallback to native region behavior when
;; required.
;;
;; Performance and Resource Control
;;
;; Apply runtime optimizations to minimize overhead:
;;
;; - Buffer-local hooks: Add hooks (e.g. `pre-command-hook') locally
;;   and deactivate them when not in use.
;;
;; - Deterministic overlay lifecycle: Create overlays on demand and
;;   remove them explicitly to avoid lingering artifacts.
;;
;; - Cached position tracking: Update buffer positions
;;   transactionally. Avoid repeated calls to `overlay-start' and
;;   `overlay-end' during high-frequency commands.


;;; Code:

(require 'mwheel)
(require 'delsel)

;;;; Customization

(defgroup dotemacs-mouse nil
  "Persistent selection."
  :group 'mouse
  :prefix "dotemacs-mouse-")

(defcustom dotemacs-mouse-overlay-priority 1001
  "Overlay priority used for persistent selections created by `dotemacs-mouse'.

The value must exceed typical overlay priorities (e.g. 100–500) to
guarantee selection visibility when other overlays coexist in the same
region."
  :type 'integer
  :group 'dotemacs-mouse)

(defcustom dotemacs-mouse-overlay-face 'region
  "Face used for persistent selection overlays managed by `dotemacs-mouse'.

Defaults to the `region' face to match standard Emacs selection
appearance.  Users may override this to differentiate persistent
selections vs. standard transient selections."
  :type 'face
  :group 'dotemacs-mouse)

(defcustom dotemacs-mouse-preview-length 40
  "Length limit for previewing the persistent selection in the
 minibuffer.

This value controls the number of characters displayed when previewing a
selection.  If the selection exceeds this length, it will be truncated
and followed by an ellipsis (`...')."
  :type 'integer
  :group 'dotemacs-mouse)

;;;; Variables

(defvar-local dotemacs-mouse--region-begin nil
  "Buffer position indicating the start of the persistent selection.

Stored as an integer rather than a marker to prevent unintended movement
during buffer modifications.")

(defvar-local dotemacs-mouse--region-end nil
  "Buffer position indicating the end of the persistent selection.

Stored as an integer rather than a marker to prevent unintended movement
during buffer modifications.")

(defvar-local dotemacs-mouse--overlays nil
  "List of overlays currently managed by dotemacs-mouse.

Maintained as a list to support potential future extensions for multiple
selection regions or complex selection shapes.")

(defvar dotemacs-mouse--typing-commands
  '(self-insert-command)
  "Commands that should replace the persistent selection on execution.

Each command in this list performs insertion, and must clear the active
selection by jumping to the overlay-defined region and deleting it.")

(defvar dotemacs-mouse--mouse-drag-commands
  '(mouse-drag-region
     mouse-set-region
     mouse-set-point
     mouse-start-rectangle
     mouse-save-then-kill)
  "Commands that should clear the persistent selection on drag operations.

These commands are typically used for dragging, and clearing the
persistent selection prevents conflicts with mouse-driven text
manipulation commands.")

;;;; Utility functions

(defun dotemacs-mouse//create-overlay (begin end)
  "Create persistent selection overlay from BEGIN and END positions."
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'priority dotemacs-mouse-overlay-priority)
    (overlay-put overlay 'face dotemacs-mouse-overlay-face)
    (overlay-put overlay 'dotemacs-mouse t)
    (push overlay dotemacs-mouse--overlays)
    overlay))

(defun dotemacs-mouse//clear-overlays ()
  "Clear persistent selection overlays from `dotemacs-mouse--overlays'."
  (dolist (overlay dotemacs-mouse--overlays)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq dotemacs-mouse--overlays nil))

(defun dotemacs-mouse//format-region-preview (begin end)
  "Format persistent selection overlay region preview between BEGIN and END
positions.

NOTE: This maintains context by showing the beginning of the selection
rather than an arbitrary middle segment."
  (let* ((text (buffer-substring-no-properties begin end))
          (len (length text))
          (preview (truncate-string-to-width text dotemacs-mouse-preview-length)))
    (if (< len dotemacs-mouse-preview-length)
      preview
      (concat preview "..."))))

(defun dotemacs-mouse//clear-if-drag-starts ()
  "Clear persistent selection overlays on new drag region."
  (when (and (memq this-command dotemacs-mouse--mouse-drag-commands)
          dotemacs-mouse--region-begin
          dotemacs-mouse--region-end)
    (dotemacs-mouse//clear-persistent-selection)))

(defun dotemacs-mouse//jump-to-overlay-position ()
  "Jump to persistent selection overlay position on
`dotemacs-mouse--typing-commands' activation.

NOTE: This function force the transition from overlay-based selection to
an active region and integrate directly with `delete-selection-mode'."
  (when (and (memq this-command dotemacs-mouse--typing-commands)
          dotemacs-mouse--region-begin
          dotemacs-mouse--region-end)
    (let* ((begin dotemacs-mouse--region-begin)
            (end dotemacs-mouse--region-end)
            (to-be-deleted-text (buffer-substring-no-properties begin end)))
      (dotemacs-mouse//clear-persistent-selection)
      (goto-char begin)
      (unless transient-mark-mode
        (setq-local transient-mark-mode t))
      ;; Simulate delete selection behavior.
      (let ((delete-selection-helper
              (lambda ()
                (when (and delete-selection-mode
                        (memq this-command dotemacs-mouse--typing-commands))
                  (delete-region begin end)
                  ;; Make sure we stay at the beginning of where the region was
                  (goto-char begin)))))
        (add-hook 'pre-command-hook delete-selection-helper nil t)
        (add-hook 'post-command-hook
          (lambda ()
            (remove-hook 'pre-command-hook delete-selection-helper t))
          nil t)))))

;;;; Interactive commands

;;;###autoload
(defun dotemacs-mouse/toggle-selection-mode ()
  "Toggle `dotemacs-mouse' for the current buffer."
  (interactive)
  (if dotemacs-mouse--region-begin
    (dotemacs-mouse//clear-persistent-selection)
    (dotemacs-mouse/activate-mark)))

;;;###autoload
(defun dotemacs-mouse/activate-mark ()
  "Activate and maintain a persistent mark using overlays.

This analyzes the current selection state to determine the most
appropriate action based on context. If a region is already active, it
will be converted to a persistent selection using overlays. If a
persistent selection already exists, it will be updated to reflect the
current region."
  (interactive)
  (cond
    ;; Case 1: Active region, no persistent selection
    ((and (region-active-p)
       (not dotemacs-mouse--region-begin)
       (not dotemacs-mouse--region-end))
      (dotemacs-mouse//set-persistent-selection (region-beginning) (region-end))
      (deactivate-mark t))

    ;; Case 2: Active region, existing persistent selection
    ((and (region-active-p)
       dotemacs-mouse--region-begin
       dotemacs-mouse--region-end)
      (dotemacs-mouse//clear-overlays)
      (dotemacs-mouse//set-persistent-selection (region-beginning) (region-end))
      (deactivate-mark t))

    ;; Case 3: No active region, inside existing persistent selection
    ((and (not (region-active-p))
       dotemacs-mouse--region-begin
       dotemacs-mouse--region-end
       (<= dotemacs-mouse--region-begin (point))
       (>= dotemacs-mouse--region-end (point)))
      (dotemacs-mouse//handle-selection-modification))

    ;; Case 4: No active region, outside existing persistent selection
    ((and (not (region-active-p))
       dotemacs-mouse--region-begin
       dotemacs-mouse--region-end
       (or (< (point) dotemacs-mouse--region-begin)
         (> (point) dotemacs-mouse--region-end)))
      (dotemacs-mouse//clear-persistent-selection))

    ;; Default case: Normal mark behavior
    (t
      (set-mark-command nil))))

;;;###autoload
(defun dotemacs-mouse/copy-selection ()
  "Copy the persistent selection or active region to the kill ring.

NOTE: This will clear the persistent selection after the copy operation
to finalize the selection cycle."
  (interactive)
  (cond
    ;; Case 1: Active region and persistent selection - update persistent selection
    ((and (region-active-p)
       dotemacs-mouse--region-begin
       dotemacs-mouse--region-end)
      (dotemacs-mouse//clear-overlays)
      (setq dotemacs-mouse--region-begin (region-beginning)
        dotemacs-mouse--region-end (region-end)))

    ;; Case 2: No active region but persistent selection exists
    ((and (not (region-active-p))
       dotemacs-mouse--region-begin
       dotemacs-mouse--region-end)
      nil) ; Just proceed to copy

    ;; Case 3: Active region but no persistent selection
    ((and (region-active-p)
       (not dotemacs-mouse--region-begin)
       (not dotemacs-mouse--region-end))
      (setq dotemacs-mouse--region-begin (region-beginning)
        dotemacs-mouse--region-end (region-end))))

  ;; Case 4: Copy the selection if it exists
  (if (and dotemacs-mouse--region-begin dotemacs-mouse--region-end)
    (progn
      (copy-region-as-kill dotemacs-mouse--region-begin dotemacs-mouse--region-end)
      (message "Copied: %s"
        (dotemacs-mouse//format-region-preview
          dotemacs-mouse--region-begin
          dotemacs-mouse--region-end))
      (dotemacs-mouse//clear-persistent-selection))
    (message "No selection to copy")))

;;;###autoload
(defun dotemacs-mouse/scroll (event)
  "Handle mouse wheel scroll EVENT while preserving region selection.

This serves as a direct replacement for `mwheel-scroll', ensuring that
selections are maintained during scroll events. It computes the
appropriate scroll amount based on mouse wheel speed and event type and
scrolls the window accordingly, whether up, down, left, or right."
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

    ;; TODO: This handles too many cases. Refactor the scrolling logic
    ;; into smaller, more focused functions, each handling a specific
    ;; case (e.g., scroll amount calculation, button-specific
    ;; scrolling actions, region handling, ...)

    ;; Case 1: Preserve selection information if active
    (with-current-buffer buffer
      (when (region-active-p)
        (setq original-point (point))
        (setq dotemacs-mouse--region-begin (region-beginning)
          dotemacs-mouse--region-end (region-end))))

    ;; Case 2: Determine the scroll amount based on event modifiers
    (let* ((mods (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
            (amt-assoc (assoc mods mouse-wheel-scroll-amount)))
      (if amt-assoc
        (setq scroll-amount (cdr amt-assoc))
        (setq scroll-amount (or (car mouse-wheel-scroll-amount) 0)))

      (when (floatp scroll-amount)
        (setq scroll-amount (1+ (truncate (* scroll-amount (window-height))))))
      (when (and mouse-wheel-progressive-speed (numberp scroll-amount))
        (setq scroll-amount (* scroll-amount (event-click-count event)))))

    ;; Case 3: Perform scrolling action based on event button
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
              (mwheel-scroll event)
              (error nil)))))

      ;; Case 4: Restore the previously selected window if necessary
      (when (and mouse-wheel-follow-mouse
              (not (eq current-window selected-window))
              (window-live-p selected-window))
        (select-window selected-window)))

    ;; Case 5: Reapply selection overlays after scroll
    (with-current-buffer buffer
      (when (and original-point
              dotemacs-mouse--region-begin
              dotemacs-mouse--region-end
              (/= (point) original-point))
        (dotemacs-mouse//clear-overlays)
        (dotemacs-mouse//create-overlay dotemacs-mouse--region-begin dotemacs-mouse--region-end)
        (deactivate-mark t)))

    ;; Case 6: Handle click events during scroll if needed
    (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
      (if mwheel-inhibit-click-event-timer
        (cancel-timer mwheel-inhibit-click-event-timer)
        (add-hook 'pre-command-hook 'mwheel-filter-click-events))
      (setq mwheel-inhibit-click-event-timer
        (run-with-timer mouse-wheel-inhibit-click-time nil
          'mwheel-inhibit-click-timeout)))))

;;;; Private functions

(defun dotemacs-mouse//set-persistent-selection (begin end)
  "Setup persistent selection between BEGIN and END positions.

This creates the core state that enables selection persistence, and
stores both the raw position values for the corresponding visual
overlay."
  (setq dotemacs-mouse--region-begin begin
    dotemacs-mouse--region-end end)
  (dotemacs-mouse//clear-overlays)
  (dotemacs-mouse//create-overlay begin end))

(defun dotemacs-mouse//clear-persistent-selection ()
  "Clear current persistent selection.

Performs a complete state reset by removing both the visual
representation and the underlying position data."
  (dotemacs-mouse//clear-overlays)
  (setq dotemacs-mouse--region-begin nil
    dotemacs-mouse--region-end nil))

;; TODO: Deprecate?
(defun dotemacs-mouse//handle-selection-modification ()
  "Interactive selection manipulation when the point is inside an existing
selection."
  (let ((options '((?b . "begin") (?e . "end") (?c . "current") (?d . "deactivate")))
         choice)
    (message "Modify selection: [b]egin | [e]nd | [c]urrent | [d]eactivate")
    (setq choice (read-char-exclusive))
    (cond
      ((eq choice ?b)
        (set-marker (mark-marker) dotemacs-mouse--region-begin (current-buffer))
        (setq mark-active t)
        (dotemacs-mouse//clear-overlays))

      ((eq choice ?e)
        (set-marker (mark-marker) dotemacs-mouse--region-end (current-buffer))
        (setq mark-active t)
        (dotemacs-mouse//clear-overlays))

      ((eq choice ?c)
        (set-marker (mark-marker) (point) (current-buffer))
        (setq mark-active t)
        (dotemacs-mouse//clear-overlays))

      ((eq choice ?d)
        (dotemacs-mouse//clear-persistent-selection))

      (t
        (message "Cancelled")))))

(defun dotemacs-mouse//handle-mouse-drag (event)
  "Handle mouse drag events by clearing existing overlays.

This serves as an event interceptor that ensures clean state transitions
when starting a new selection. It prevents the common problem of
'orphaned' visual selections that remain visible but are no longer
connected to any actual region state."
  (interactive "e")
  (dotemacs-mouse//clear-persistent-selection)
  (mouse-drag-region event))

;;;; Setup functions

(defun dotemacs-mouse//self-insert-advice (orig-fun &rest args)
  "Advice for `self-insert-command' to handle overlay selections."
  (if (and dotemacs-mouse--region-begin
        dotemacs-mouse--region-end)
    (let ((beg dotemacs-mouse--region-begin)
           (end dotemacs-mouse--region-end))
      (dotemacs-mouse//clear-persistent-selection)
      (delete-region beg end)
      (goto-char beg)
      (apply orig-fun args))
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode dotemacs-mouse/selection-mode
  "Toggle persistent selection when scrolling with the mouse."
  :lighter " DM"
  :global nil
  :group 'dotemacs-mouse
  (if dotemacs-mouse/selection-mode
    (progn
      (add-hook 'pre-command-hook 'dotemacs-mouse//clear-if-drag-starts nil t)
      (advice-add 'self-insert-command :around #'dotemacs-mouse//self-insert-advice)
      (dotemacs-mouse//setup-mouse-bindings)
      (unless delete-selection-mode
        (delete-selection-mode 1)))
    (dotemacs-mouse//clear-persistent-selection)
    (remove-hook 'pre-command-hook 'dotemacs-mouse//clear-if-drag-starts t)
    (advice-remove 'self-insert-command #'dotemacs-mouse//self-insert-advice)
    (dotemacs-mouse//restore-mouse-bindings)))

;;;###autoload
(define-globalized-minor-mode global-dotemacs-mouse/selection-mode
  dotemacs-mouse/selection-mode
  (lambda ()
    (dotemacs-mouse/selection-mode 1))
  :group 'dotemacs-mouse)

(defun dotemacs-mouse//setup-mouse-bindings ()
  "Setup mouse bindings for selection-preserving scrolling."
  (global-set-key [remap mwheel-scroll] 'dotemacs-mouse/scroll)
  (global-set-key [(mouse-4)] 'dotemacs-mouse/scroll)
  (global-set-key [(mouse-5)] 'dotemacs-mouse/scroll)
  (global-set-key [(control mouse-4)] 'dotemacs-mouse/scroll)
  (global-set-key [(control mouse-5)] 'dotemacs-mouse/scroll)
  (global-set-key [(shift mouse-4)] 'dotemacs-mouse/scroll)
  (global-set-key [(shift mouse-5)] 'dotemacs-mouse/scroll)
  (global-set-key [down-mouse-1] 'dotemacs-mouse//handle-mouse-drag))

(defun dotemacs-mouse//restore-mouse-bindings ()
  "Restore original mouse bindings."
  (global-set-key [remap dotemacs-mouse/scroll] 'mwheel-scroll)
  (global-set-key [(mouse-4)] 'mwheel-scroll)
  (global-set-key [(mouse-5)] 'mwheel-scroll)
  (global-set-key [(control mouse-4)] 'mwheel-scroll)
  (global-set-key [(control mouse-5)] 'mwheel-scroll)
  (global-set-key [(shift mouse-4)] 'mwheel-scroll)
  (global-set-key [(shift mouse-5)] 'mwheel-scroll)
  (global-set-key [down-mouse-1] 'mouse-drag-region))

(put 'dotemacs-mouse/scroll 'scroll-command t)

(provide 'dotemacs-mouse)

;;; dotemacs-mouse.el ends here
