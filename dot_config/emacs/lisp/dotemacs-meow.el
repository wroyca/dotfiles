;;; dotemacs-meow.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;


;;; Code:

(require 'meow)
(require 'cl-lib)

(defgroup dotemacs-meow nil
  "Meow configuration."
  :group 'editing
  :prefix "dotemacs-meow-")

;;; Custom Variables

(defcustom dotemacs-meow-operator-timeout 2.0
  "Maximum time in seconds to wait in operator-pending state.

If no motion is received within this interval, the pending operator
state is aborted and the internal state is reset. This prevents
lingering state from interfering with subsequent modal commands.

The value must be a non-negative number representing seconds."
  :type 'number
  :group 'dotemacs-meow)

;;; Internal Variables

(defvar dotemacs-meow--operator-state nil
  "Current operator state during modal composition.

When non-nil, this is a cons cell of the form (OPERATOR . COUNT), where:
- OPERATOR is a function of the form (lambda (BEG END) ...), to be applied to the region
- COUNT is an integer prefix argument, or nil if no prefix was supplied")

(defvar dotemacs-meow--count nil
  "Current numeric prefix argument in modal state.

This variable is used to propagate `count' values during multi-stage
modal command composition.")

(defvar dotemacs-meow--last-command nil
  "Last executed modal command in Meow normal mode.

Used for repeat functionality or analysis of operator sequences.")

(defvar dotemacs-meow--operator-timer nil
  "Timer object used to enforce `dotemacs-meow-operator-timeout'.

When active, the timer cancels the pending operator state upon
expiration.")

;;; Utility Functions

(defun dotemacs-meow--cleanup-operator-state ()
  "Cancel any pending operator state and associated timeout.

This function ensures the internal state is reset to avoid interference
with further commands."
  (when dotemacs-meow--operator-timer
    (cancel-timer dotemacs-meow--operator-timer)
    (setq dotemacs-meow--operator-timer nil))
  (setq dotemacs-meow--operator-state nil))


(defun dotemacs-meow--report-error (err)
  "Display error message for ERR and reset transient modal state.

This function is invoked on operator failure or unexpected command errors."
  (dotemacs-meow--cleanup-operator-state)
  (message "Meow error: %s" (error-message-string err)))

;;; Core Command Implementations

(defun dotemacs-meow-delete-char ()
  "Delete the character at point.

Performs a forward deletion using `delete-char'. If point is at the end
of buffer (`eobp'), no action is performed."
  (interactive)
  (condition-case err
      (when (not (eobp))
        (delete-char 1))
    (error (dotemacs-meow--report-error err))))

(defun dotemacs-meow-backward-delete-char ()
  "Delete the character before point.

Performs a backward deletion using `delete-char' with a negative
argument. If point is at the beginning of buffer (`bobp'), no action is
performed."
  (interactive)
  (condition-case err
      (when (not (bobp))
        (delete-char -1))
    (error (dotemacs-meow--report-error err))))

(defun dotemacs-meow-paste-before ()
  "Insert the most recent kill from `kill-ring' before point.

Retrieves the current kill via `current-kill'. If the retrieved text is
non-empty, it is inserted before point using `insert' within a
`save-excursion' form to preserve point position."
  (interactive)
  (condition-case err
      (let ((text (current-kill 0 t)))
        (when (and text (not (string-empty-p text)))
          (save-excursion
            (insert text))))
    (error (dotemacs-meow--report-error err))))

(defun dotemacs-meow-paste-after ()
  "Insert the most recent kill from `kill-ring' at point.

Retrieves the current kill via `current-kill'. If the retrieved text is
non-empty, it is inserted at point using `insert'. Point advances as a
result of the insertion."
  (interactive)
  (condition-case err
      (let ((text (current-kill 0 t)))
        (when (and text (not (string-empty-p text)))
          (insert text)))
    (error (dotemacs-meow--report-error err))))

;;; Operator System

(defun dotemacs-meow--handle-operator (op)
  "Begin or complete modal operator OP.

If OP matches the current operator state, the operator is applied to the
current line (e.g. repeating `d' to execute `dd'). Otherwise, the editor
enters an operator-pending state, awaiting a motion command.

OP must be a function of the form (lambda (BEG END) ...)."
  (condition-case err
      (let ((count (or current-prefix-arg 1)))
        (if (and dotemacs-meow--operator-state
                 (eq (car dotemacs-meow--operator-state) op))
            (progn
              (let* ((count (or (cdr dotemacs-meow--operator-state) count))
                     (beg (line-beginning-position))
                     (end (save-excursion
                           (forward-line count)
                           (point))))
                (setq dotemacs-meow--operator-state nil)
                (funcall op beg end)))
          (setq dotemacs-meow--operator-state (cons op count))
          (set-transient-map
           (dotemacs-meow--create-operator-map op))))
    (error (setq dotemacs-meow--operator-state nil))))

(defun dotemacs-meow--create-operator-map (op)
  "Construct a transient keymap for operator OP.

The returned keymap binds motion keys to functions that apply OP to the
region defined by the corresponding motion command.

OP must be a function of the form (lambda (BEG END) ...)."
  (let ((map (make-sparse-keymap)))
    (dolist (motion '(("j" . meow-next)
                     ("k" . meow-prev)
                     ("h" . meow-left)
                     ("l" . meow-right)
                     ("w" . meow-next-word)
                     ("b" . meow-back-word)
                     ("e" . meow-next-symbol)))
      (define-key map (kbd (car motion))
        (lambda ()
          (interactive)
          (dotemacs-meow--execute-operator-motion op (cdr motion)))))
    map))

(defun dotemacs-meow--execute-operator-motion (op motion)
  "Apply operator OP to the region defined by MOTION.

This function:
1. Records point position as the region start
2. Executes MOTION with count (if present)
3. Calls OP with the region (BEG END)

OP must accept two region arguments.
MOTION must be an interactive function."
  (condition-case err
      (let* ((op-state dotemacs-meow--operator-state)
             (op-count (cdr op-state))
             (count (or dotemacs-meow--count op-count 1))
             (start-pos (point)))
        (setq dotemacs-meow--operator-state nil
              dotemacs-meow--count nil)
        (let ((current-prefix-arg count))
          (call-interactively motion))
        (funcall op start-pos (point)))
    (error
     (setq dotemacs-meow--count nil))))

;;; Commands with Operator Support

(defun dotemacs-meow-kill-region-or-operator ()
  "Kill active region or enter `kill-region' as modal operator.

If a region is active, `kill-region' is invoked directly.  Otherwise,
enter operator-pending state expecting a motion, which will determine
the region to kill, e.g. `d d' to delete a line."
  (interactive)
  (condition-case err
      (if (region-active-p)
          (kill-region (region-beginning) (region-end))
        (dotemacs-meow--handle-operator #'kill-region))
    (error (dotemacs-meow--report-error err))))

(defun dotemacs-meow-change-region-or-operator ()
  "Change region or enter change operator mode.

If a region is active, the region is killed and the editor enters insert
mode.  Otherwise, enter operator-pending state using an anonymous
operator function that performs `kill-region' followed by `meow-insert'.

Semantically similar to Vi's `c' operator."
  (interactive)
  (condition-case err
      (if (region-active-p)
          (progn
            (kill-region (region-beginning) (region-end))
            (meow-insert))
        (dotemacs-meow--handle-operator
         (lambda (beg end)
           (kill-region beg end)
           (meow-insert))))
    (error (dotemacs-meow--report-error err))))

;;; Keybinding Setup

(defun dotemacs-meow-setup-keys ()
  "Define Meow keybindings for normal mode.

Binds modal editing commands for insertion, motion, operator dispatch,
and undo operations."
  (meow-normal-define-key
   ;; Insert & Open
   '("i" . meow-insert)
   '("a" . meow-append)
   '("o" . meow-open-below)
   '("O" . meow-open-above)

   ;; Word Movement
   '("w" . meow-next-word)
   '("b" . meow-back-word)
   '("e" . meow-next-symbol)

   ;; Operators & Actions
   '("d" . dotemacs-meow-kill-region-or-operator)
   '("c" . dotemacs-meow-change-region-or-operator)
   '("y" . meow-save)
   '("p" . dotemacs-meow-paste-after)
   '("P" . dotemacs-meow-paste-before)

   ;; Undo System
   '("u" . undo)))

;;; Keymap Hierarchy

(defmacro define-prefix-keymap (var-name prefix description &rest bindings)
  "Define a prefix keymap named VAR-NAME, bound to PREFIX, with DESCRIPTION for `which-key'.

This macro sets up:
- A named keymap variable `VAR-NAME'
- Global binding of PREFIX to that keymap
- Descriptive labels for keys using `which-key'
- Recursive support for nested sub-keymaps, with qualified prefixes

Each entry in BINDINGS is one of:

  (KEY FUNCTION DESCRIPTION)
    or
  (KEY DESCRIPTION NESTED-BINDINGS...)

KEY is a string (as accepted by `kbd'), FUNCTION is a command or lambda,
and DESCRIPTION is a string label for `which-key'. In the nested form,
NESTED-BINDINGS describes a submenu with its own prefix.

This macro is especially useful when used with Meow, as
`which-key-add-key-based-replacements' often fails to display nested key
descriptions correctly in Meow's key dispatch model.

Example:

  (define-prefix-keymap my-buffer-keymap \"C-c b\" \"Buffer\"
    (\"b\" switch-to-buffer \"Switch buffer\")
    (\"k\" kill-buffer \"Kill buffer\")
    (\"r\" rename-buffer \"Rename buffer\")
    (\"l\" list-buffers \"List buffers\"))"
  (declare (indent defun))
  (let ((map-name (make-symbol "map")))
    `(progn
       (defvar ,var-name nil ,(format "Keymap for %s" description))
       (let ((,map-name (make-sparse-keymap)))
         ,@(cl-loop for binding in bindings
             collect
             (pcase binding
               (`(,key ,func ,desc)
                 `(progn
                    (define-key ,map-name (kbd ,key) ,func)
                    (let ((full-key (concat ,prefix " " ,key)))
                      (with-eval-after-load 'which-key
                        (when (symbolp ,func)
                          (push (cons (cons nil (symbol-name ,func))
                                  (cons nil ,desc))
                            which-key-replacement-alist))
                        (which-key-add-key-based-replacements full-key ,desc)))))
               (`(,key ,nested-desc . ,nested-bindings)
                 (let ((nested-var (intern (format "%s-%s-keymap"
                                             (symbol-name var-name)
                                             (replace-regexp-in-string " " "-" key)))))
                   `(progn
                      (define-prefix-keymap ,nested-var
                        ,(concat prefix " " key) ,nested-desc
                        ,@nested-bindings)
                      (define-key ,map-name (kbd ,key) ,nested-var))))))
         (setq ,var-name ,map-name)
         (global-set-key (kbd ,prefix) ,var-name)
         (with-eval-after-load 'which-key
           (which-key-add-key-based-replacements ,prefix ,description)
           (which-key-add-keymap-based-replacements global-map ,prefix ,description)
           (push (cons (cons nil (symbol-name ',var-name))
                   (cons nil ,description))
             which-key-replacement-alist))
         ,var-name))))

(define-prefix-keymap my-main-keymap "C-c" "Main"

  ;; x, c, h, m, g are reserved.

  ;; File operations
  ("f" "File"
    ("f" #'find-file "Find file")
    ("s" #'save-buffer "Save buffer")
    ("S" #'write-file "Save as")
    ("r" #'recentf-open-files "Open recent file")
    ("d" #'dired-jump "Open Dired"))

  ;; Buffer operations
  ("b" "Buffer"
    ("b" #'switch-to-buffer "Switch buffer")
    ("k" #'kill-buffer "Kill buffer")
    ("r" #'rename-buffer "Rename buffer")
    ("l" #'list-buffers "List buffers"))

  ;; Window management
  ("w" "Window"
    ("v" #'split-window-right "Split vertically")
    ("s" #'split-window-below "Split horizontally")
    ("d" #'delete-window "Delete window")
    ("o" #'other-window "Other window")
    ("m" #'delete-other-windows "Maximize window"))

  ;; Code actions (general)
  ("C" "Code"
    ("r" #'eglot-rename "Rename symbol")
    ("a" #'eglot-code-actions "Code actions")
    ("f" #'eglot-format "Format buffer"))

  ;; Navigation
  ("n" "Navigation"
    ("g" #'goto-line "Go to line")
    ("m" #'imenu "Jump to symbol (imenu)")
    ("." #'xref-find-definitions "Go to definition")
    ("," #'xref-pop-marker-stack "Pop definition stack"))

  ;; Search
  ("s" "Search"
    ("s" #'isearch-forward "Search forward")
    ("r" #'isearch-backward "Search backward")
    ("g" #'consult-grep "Grep (consult)")
    ("o" #'occur "Occur")
    ("p" #'project-find-regexp "Project search"))

  ;; Project operations
  ("p" "Project"
    ("f" #'project-find-file "Find file in project")
    ("s" #'project-find-regexp "Search in project")
    ("d" #'project-dired "Dired in project")
    ("b" #'project-switch-to-buffer "Project buffer"))

  ;; Git integration
  ("G" "Git"
    ("s" #'magit-status "Magit status")
    ("l" #'magit-log-buffer-file "Log current file")
    ("b" #'magit-blame-addition "Blame current line")))

;;;###autoload
(defun dotemacs-meow-setup ()
  "Setup `dotemacs-meow' keybindings."
  (interactive)
  (dotemacs-meow-setup-keys))

(provide 'dotemacs-meow)

;;; dotemacs-meow.el ends here
