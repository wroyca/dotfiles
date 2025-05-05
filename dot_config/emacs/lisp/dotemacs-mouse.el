;;; dotemacs-mouse.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Architectural Overview
;;
;; Overlay-Based Selection Model
;;
;; Selections are implemented via overlays rather than the point/mark pair.
;; This design enables:
;;
;; - Spatial independence: Overlay visibility is decoupled from `point'.
;;   Selections remain visually active during cursor motion or scroll operations.
;;
;; - Display priority enforcement: Overlays are assigned priority 1001 to override
;;   concurrent visual properties applied by external packages.
;;
;; - Cached positional metadata: Character positions are maintained in
;;   `dotemacs-mouse--region-begin' and `dotemacs-mouse--region-end' to avoid
;;   runtime overlay introspection during command execution.
;;
;; Input Event Interception
;;
;; Existing interactive commands are intercepted using the following strategies:
;;
;; - Function remapping: Mouse-related commands (e.g. `mwheel-scroll') are remapped
;;   to wrappers that preserve overlay selections during event handling.
;;
;; - Advice instrumentation: Mutating commands such as `self-insert-command' are
;;   advised to detect active overlay regions and apply type-to-replace behavior.
;;
;; - Pre-command hook monitoring: Drag initiations and state transitions are detected
;;   via `pre-command-hook' to enable synchronized state teardown and reinitialization.
;;
;; Selection State Management
;;
;; Selection state is represented via three cooperating mechanisms:
;;
;; 1. `dotemacs-mouse--region-begin' / `dotemacs-mouse--region-end': Store absolute
;;    buffer positions as integers. Markers are avoided to prevent drift during
;;    unrelated buffer edits.
;;
;; 2. `dotemacs-mouse--overlays': List of active overlay objects defining the
;;    rendered selection.
;;
;; 3. `dotemacs-mouse-selection-mode': Minor mode flag indicating whether overlay
;;    selection functionality is active in the current buffer.
;;
;; Compatibility Layer with Emacs Primitives
;;
;; Interoperates with core selection-related subsystems:
;;
;; - `delete-selection-mode': Integrates with overwrite semantics by treating active
;;   overlays as virtual regions.
;;
;; - `transient-mark-mode': Temporarily enabled where native region behavior is
;;   required, e.g., when delegating to internal selection logic.
;;
;; - Scroll subsystem (`mwheel.el'): Mouse wheel input is processed in conformance
;;   with `scroll-command' conventions to retain compatibility with motion behavior.
;;
;; Transition Handling Between Selection Models
;;
;; Transitional logic in `dotemacs-mouse-activate-mark' detects the current
;; selection state and executes stateful transitions between overlay-backed
;; selections and point/mark-based regions. This permits seamless fallback
;; to native behavior when required.
;;
;; Performance and Resource Control
;;
;; Optimizations are applied to minimize runtime overhead:
;;
;; - Localized hooks: `pre-command-hook' and others are added buffer-locally and
;;   deactivated when not in use.
;;
;; - Overlay lifecycle control: Overlay creation is demand-driven. Cleanup occurs
;;   deterministically to avoid orphaned overlays.
;;
;; - Cached position tracking: Buffer positions are updated transactionally to
;;   avoid repeated calls to `overlay-start'/`overlay-end' during high-frequency
;;   operations.


;;; Code:

(require 'mwheel)
(require 'delsel)

;;; Customization

(defgroup dotemacs-mouse nil
  "Selection persistence."
  :group 'mouse
  :prefix "dotemacs-mouse-")

(defcustom dotemacs-mouse-overlay-priority 1001
  "Priority for overlays created by dotemacs-mouse.
Higher values appear above other overlays."
  :type 'integer
  :group 'dotemacs-mouse)

(defcustom dotemacs-mouse-overlay-face 'region
  "Face used for selection overlays."
  :type 'face
  :group 'dotemacs-mouse)

(defcustom dotemacs-mouse-preview-length 40
  "Maximum length for region preview in messages."
  :type 'integer
  :group 'dotemacs-mouse)

;;; Variables

(defvar-local dotemacs-mouse--region-begin nil
  "The beginning position of the persistent selection.")

(defvar-local dotemacs-mouse--region-end nil
  "The ending position of the persistent selection.")

(defvar-local dotemacs-mouse--overlays nil
  "List of overlays currently managed by dotemacs-mouse.")

(defvar dotemacs-mouse--typing-commands
  '(self-insert-command)
  "List of commands that should trigger a jump to the overlay position.")

(defvar dotemacs-mouse--mouse-drag-commands
  '(mouse-drag-region 
    mouse-set-region 
    mouse-set-point
    mouse-start-rectangle
    mouse-save-then-kill)
  "List of commands that start drag operations.")

;;; Utility functions

(defun dotemacs-mouse--create-overlay (begin end)
  "Create a dotemacs-mouse overlay between BEGIN and END positions."
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'priority dotemacs-mouse-overlay-priority)
    (overlay-put overlay 'face dotemacs-mouse-overlay-face)
    (overlay-put overlay 'dotemacs-mouse t)
    (push overlay dotemacs-mouse--overlays)
    overlay))

(defun dotemacs-mouse--clear-overlays ()
  "Remove all dotemacs-mouse overlays in the current buffer."
  (dolist (overlay dotemacs-mouse--overlays)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq dotemacs-mouse--overlays nil))

(defun dotemacs-mouse--format-region-preview (begin end)
  "Format a preview of the region between BEGIN and END positions."
  (let* ((text (buffer-substring-no-properties begin end))
         (len (length text))
         (preview (truncate-string-to-width text dotemacs-mouse-preview-length)))
    (if (< len dotemacs-mouse-preview-length)
        preview
      (concat preview "..."))))

(defun dotemacs-mouse--clear-if-drag-starts ()
  "Clear overlays if a new drag region is starting."
  (when (and (memq this-command dotemacs-mouse--mouse-drag-commands)
             dotemacs-mouse--region-begin 
             dotemacs-mouse--region-end)
    (dotemacs-mouse--clear-persistent-selection)))

(defun dotemacs-mouse--jump-to-overlay-position ()
  "Jump to the overlay position if a typing command is about to be executed.
Activate the region so delete-selection-mode will replace it with typed text."
  (when (and (memq this-command dotemacs-mouse--typing-commands)
             dotemacs-mouse--region-begin
             dotemacs-mouse--region-end)
    (let* ((begin dotemacs-mouse--region-begin)
           (end dotemacs-mouse--region-end)
           (to-be-deleted-text (buffer-substring-no-properties begin end)))
      (dotemacs-mouse--clear-persistent-selection)
      (goto-char begin)
      (unless transient-mark-mode
        (setq-local transient-mark-mode t))
      ;; Simulate delete selection behavior
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

;;; Interactive commands

;;;###autoload
(defun dotemacs-mouse-toggle-selection-mode ()
  "Toggle the dotemacs-mouse selection mode for the current buffer."
  (interactive)
  (if dotemacs-mouse--region-begin
      (dotemacs-mouse--clear-persistent-selection)
    (dotemacs-mouse-activate-mark)))

;;;###autoload
(defun dotemacs-mouse-activate-mark ()
  "Activate and maintain a persistent mark using overlays.
If a region is already active, it will be converted to a persistent selection.
If a persistent selection already exists, it will be updated."
  (interactive)
  (cond
   ;; Case 1: Active region, no persistent selection
   ((and (region-active-p)
         (not dotemacs-mouse--region-begin)
         (not dotemacs-mouse--region-end))
    (dotemacs-mouse--set-persistent-selection (region-beginning) (region-end))
    (deactivate-mark t))
   
   ;; Case 2: Active region, existing persistent selection
   ((and (region-active-p)
         dotemacs-mouse--region-begin
         dotemacs-mouse--region-end)
    (dotemacs-mouse--clear-overlays)
    (dotemacs-mouse--set-persistent-selection (region-beginning) (region-end))
    (deactivate-mark t))
   
   ;; Case 3: No active region, inside existing persistent selection
   ((and (not (region-active-p))
         dotemacs-mouse--region-begin
         dotemacs-mouse--region-end
         (<= dotemacs-mouse--region-begin (point))
         (>= dotemacs-mouse--region-end (point)))
    (dotemacs-mouse--handle-selection-modification))
   
   ;; Case 4: No active region, outside existing persistent selection
   ((and (not (region-active-p))
         dotemacs-mouse--region-begin
         dotemacs-mouse--region-end
         (or (< (point) dotemacs-mouse--region-begin)
             (> (point) dotemacs-mouse--region-end)))
    (dotemacs-mouse--clear-persistent-selection))
   
   ;; Default case: Normal mark behavior
   (t
    (set-mark-command nil))))

;;;###autoload
(defun dotemacs-mouse-copy-selection ()
  "Copy the persistent selection to the kill ring.
If no persistent selection exists but a region is active, copy that instead."
  (interactive)
  (cond
   ;; Active region and persistent selection - update persistent selection
   ((and (region-active-p)
         dotemacs-mouse--region-begin
         dotemacs-mouse--region-end)
    (dotemacs-mouse--clear-overlays)
    (setq dotemacs-mouse--region-begin (region-beginning)
          dotemacs-mouse--region-end (region-end)))
   
   ;; No active region but persistent selection exists
   ((and (not (region-active-p))
         dotemacs-mouse--region-begin
         dotemacs-mouse--region-end)
    nil) ; Just proceed to copy
   
   ;; Active region but no persistent selection
   ((and (region-active-p)
         (not dotemacs-mouse--region-begin)
         (not dotemacs-mouse--region-end))
    (setq dotemacs-mouse--region-begin (region-beginning)
          dotemacs-mouse--region-end (region-end))))
  
  ;; Copy the selection if it exists
  (if (and dotemacs-mouse--region-begin dotemacs-mouse--region-end)
      (progn
        (copy-region-as-kill dotemacs-mouse--region-begin dotemacs-mouse--region-end)
        (message "Copied: %s"
                 (dotemacs-mouse--format-region-preview
                  dotemacs-mouse--region-begin
                  dotemacs-mouse--region-end))
        (dotemacs-mouse--clear-persistent-selection))
    (message "No selection to copy")))

;;;###autoload
(defun dotemacs-mouse-scroll (event)
  "Scroll window according to mouse wheel EVENT while preserving selection.
This is a replacement for `mwheel-scroll' that maintains selections."
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
    
    ;; Save region information if active
    (with-current-buffer buffer
      (when (region-active-p)
        (setq original-point (point))
        (setq dotemacs-mouse--region-begin (region-beginning)
              dotemacs-mouse--region-end (region-end))))
    
    ;; Calculate scroll amount
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
    
    ;; Perform the scroll
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
           
           ;; Handle horizontal scroll and other events
           ((or (eq button 'wheel-right) (eq button mouse-wheel-right-event))
            (when (boundp 'mwheel-scroll-right-function)
              (funcall mwheel-scroll-right-function scroll-amount)))
           
           ((or (eq button 'wheel-left) (eq button mouse-wheel-left-event))
            (when (boundp 'mwheel-scroll-left-function)
              (funcall mwheel-scroll-left-function scroll-amount)))
           
           ;; Fall back to standard mwheel-scroll for unsupported event types
           (t 
            (condition-case nil
                (mwheel-scroll event)
              (error nil)))))
      
      ;; Restore window selection if needed
      (when (and mouse-wheel-follow-mouse
                 (not (eq current-window selected-window))
                 (window-live-p selected-window))
        (select-window selected-window)))
    
    ;; Restore and maintain selection
    (with-current-buffer buffer
      (when (and original-point
                 dotemacs-mouse--region-begin
                 dotemacs-mouse--region-end
                 (/= (point) original-point))
        (dotemacs-mouse--clear-overlays)
        (dotemacs-mouse--create-overlay dotemacs-mouse--region-begin dotemacs-mouse--region-end)
        (deactivate-mark t)))
    
    ;; Handle mouse wheel timers (from original mwheel-scroll)
    (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
      (if mwheel-inhibit-click-event-timer
          (cancel-timer mwheel-inhibit-click-event-timer)
        (add-hook 'pre-command-hook 'mwheel-filter-click-events))
      (setq mwheel-inhibit-click-event-timer
            (run-with-timer mouse-wheel-inhibit-click-time nil
                            'mwheel-inhibit-click-timeout)))))

;;; Private functions

(defun dotemacs-mouse--set-persistent-selection (begin end)
  "Set up a persistent selection between BEGIN and END positions."
  (setq dotemacs-mouse--region-begin begin
        dotemacs-mouse--region-end end)
  (dotemacs-mouse--clear-overlays)
  (dotemacs-mouse--create-overlay begin end))

(defun dotemacs-mouse--clear-persistent-selection ()
  "Clear the current persistent selection."
  (dotemacs-mouse--clear-overlays)
  (setq dotemacs-mouse--region-begin nil
        dotemacs-mouse--region-end nil))

(defun dotemacs-mouse--handle-selection-modification ()
  "Handle modification or adjustment of the current persistent selection."
  (let ((options '((?b . "begin") (?e . "end") (?c . "current") (?d . "deactivate")))
        choice)
    (message "Modify selection: [b]egin | [e]nd | [c]urrent | [d]eactivate")
    (setq choice (read-char-exclusive))
    (cond
     ((eq choice ?b)
      (set-marker (mark-marker) dotemacs-mouse--region-begin (current-buffer))
      (setq mark-active t)
      (dotemacs-mouse--clear-overlays))
     
     ((eq choice ?e)
      (set-marker (mark-marker) dotemacs-mouse--region-end (current-buffer))
      (setq mark-active t)
      (dotemacs-mouse--clear-overlays))
     
     ((eq choice ?c)
      (set-marker (mark-marker) (point) (current-buffer))
      (setq mark-active t)
      (dotemacs-mouse--clear-overlays))
     
     ((eq choice ?d)
      (dotemacs-mouse--clear-persistent-selection))
     
     (t
      (message "Cancelled")))))

(defun dotemacs-mouse--handle-mouse-drag (event)
  "Handle mouse drag events by clearing existing overlays.
Argument EVENT is the mouse event."
  (interactive "e")
  (dotemacs-mouse--clear-persistent-selection)
  (mouse-drag-region event))

;;; Setup functions

(defun dotemacs-mouse--self-insert-advice (orig-fun &rest args)
  "Advice to make self-insert replace overlay selections.
ORIG-FUN is the original function.
ARGS are the arguments to pass to it."
  (if (and dotemacs-mouse--region-begin 
           dotemacs-mouse--region-end)
      (let ((beg dotemacs-mouse--region-begin)
            (end dotemacs-mouse--region-end))
        (dotemacs-mouse--clear-persistent-selection)
        (delete-region beg end)
        (goto-char beg)
        (apply orig-fun args))
    ;; No overlay selection, just call original
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode dotemacs-mouse-selection-mode
  "Toggle persistent selection when scrolling with the mouse.
When enabled, text selections are maintained when scrolling."
  :lighter " DM"
  :global nil
  :group 'dotemacs-mouse
  (if dotemacs-mouse-selection-mode
      (progn
        ;; Register lifecycle events
        (add-hook 'pre-command-hook 'dotemacs-mouse--clear-if-drag-starts nil t)
        (advice-add 'self-insert-command :around #'dotemacs-mouse--self-insert-advice)
        (dotemacs-mouse--setup-mouse-bindings)
        (unless delete-selection-mode
          (delete-selection-mode 1)))
    ;; Deregistration to ensure clean state
    (dotemacs-mouse--clear-persistent-selection)
    (remove-hook 'pre-command-hook 'dotemacs-mouse--clear-if-drag-starts t)
    (advice-remove 'self-insert-command #'dotemacs-mouse--self-insert-advice)
    (dotemacs-mouse--restore-mouse-bindings)))

;;;###autoload
(define-globalized-minor-mode global-dotemacs-mouse-selection-mode
  dotemacs-mouse-selection-mode
  (lambda () (dotemacs-mouse-selection-mode 1))
  :group 'dotemacs-mouse)

(defun dotemacs-mouse--setup-mouse-bindings ()
  "Set up mouse bindings for selection-preserving scrolling."
  ;; Scroll bindings
  (global-set-key [remap mwheel-scroll] 'dotemacs-mouse-scroll)
  (global-set-key [(mouse-4)] 'dotemacs-mouse-scroll)
  (global-set-key [(mouse-5)] 'dotemacs-mouse-scroll)
  (global-set-key [(control mouse-4)] 'dotemacs-mouse-scroll)
  (global-set-key [(control mouse-5)] 'dotemacs-mouse-scroll)
  (global-set-key [(shift mouse-4)] 'dotemacs-mouse-scroll)
  (global-set-key [(shift mouse-5)] 'dotemacs-mouse-scroll)
  
  ;; Drag bindings - override mouse-drag-region to clear overlays first
  (global-set-key [down-mouse-1] 'dotemacs-mouse--handle-mouse-drag))

(defun dotemacs-mouse--restore-mouse-bindings ()
  "Restore original mouse bindings."
  ;; Restore scroll bindings
  (global-set-key [remap dotemacs-mouse-scroll] 'mwheel-scroll)
  (global-set-key [(mouse-4)] 'mwheel-scroll)
  (global-set-key [(mouse-5)] 'mwheel-scroll)
  (global-set-key [(control mouse-4)] 'mwheel-scroll)
  (global-set-key [(control mouse-5)] 'mwheel-scroll)
  (global-set-key [(shift mouse-4)] 'mwheel-scroll)
  (global-set-key [(shift mouse-5)] 'mwheel-scroll)
  
  ;; Restore drag bindings
  (global-set-key [down-mouse-1] 'mouse-drag-region))

(put 'dotemacs-mouse-scroll 'scroll-command t)

(global-dotemacs-mouse-selection-mode 1)

(provide 'dotemacs-mouse)

;;; dotemacs-mouse.el ends here 
