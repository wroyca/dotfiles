; -*- mode: emacs-lisp; lexical-binding: t -*-

(defvar lumen-background "")

(defvar lumen-exit-code -1
  "Last job exit code.")

(defvar lumen-lines nil
  "List to store output from job.")

(defvar lumen-elines nil
  "List to store error output from job.")

(defvar lumen-watched-line "/org/freedesktop/portal/desktop: org.freedesktop.portal.Settings.SettingChanged ('org.freedesktop.appearance', 'color-scheme', <uint32 "
  "Line pattern to watch for D-Bus notifications.")

(defun lumen-debug-message (msg)
  "Print if Emacs is started with the --debug-init flag."
  (when debug-on-error
    (message "Lumen debug: %s" msg)))

(defun lumen-apply-colorscheme ()
  "Apply colorscheme based on `lumen-background`."
  (let ((colorscheme
          (cond
            ((string= lumen-background "light") "modus-operandi")
            ((string= lumen-background "dark") "modus-vivendi")
            (t "")))) ;; Default to no colorscheme if not matching

    (when (and (not (string-empty-p colorscheme))
               (not (string= (car custom-enabled-themes) colorscheme)))
      (lumen-debug-message (format "Applying colorscheme: %s" colorscheme))
      (load-theme (intern colorscheme) t))))

(defun lumen-light-hook ()
  "Hook to apply light background color scheme."
  (lumen-debug-message (format "Lumen light hook: Current background %s, desired %s"
                               (frame-parameter nil 'background) "light"))
  (unless (and (string= lumen-background "light")
               (string= (frame-parameter nil 'background) lumen-background))
    (set-frame-parameter nil 'background "light")
    (setq lumen-background (frame-parameter nil 'background))
    (lumen-debug-message "Lumen light hook: Background set to light")
    (lumen-apply-colorscheme))
  (run-hooks 'lumen-light-hook))

(defun lumen-dark-hook ()
  "Hook to apply dark background color scheme."
  (lumen-debug-message (format "Lumen dark hook: Current background %s, desired %s"
                               (frame-parameter nil 'background) "dark"))
  (unless (and (string= lumen-background "dark")
               (string= (frame-parameter nil 'background) lumen-background))
    (set-frame-parameter nil 'background "dark")
    (setq lumen-background (frame-parameter nil 'background))
    (lumen-debug-message "Lumen dark hook: Background set to dark")
    (lumen-apply-colorscheme))
  (run-hooks 'lumen-dark-hook))

(defun lumen-oneshot ()
  "Run one-shot commands for Linux platforms."
  (lumen-debug-message "Lumen oneshot: Running Linux-specific one-shot command...")
  (let ((out (string-trim (shell-command-to-string
                            (format "%s call -t 1 --session --dest=org.freedesktop.portal.Desktop --object-path=/org/freedesktop/portal/desktop --method=org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme"
                                    (executable-find "gdbus"))))))
    (lumen-debug-message (format "Lumen oneshot: Output %s" out))
    (if (string-match "(<<uint32 " out)
      (lumen-platforms-linux-parse-line (concat lumen-watched-line (substring out 10 11) ">"))
      (lumen-debug-log-err out))))

(defun lumen-parse-output (line)
  "Parse output from job."
  (lumen-debug-message (format "Lumen parse output: %s" line))
  (lumen-platforms-linux-parse-line line))

(defun lumen-on-stdout (process output)
  "Handle standard output from job."
  (let ((data (split-string output "\n" t)))
    (lumen-debug-message (format "Lumen on stdout: %s" output))
    (setq lumen-lines (append lumen-lines data))
    (while (> (length lumen-lines) 1)
           (let ((line (car lumen-lines)))
             (setq lumen-lines (cdr lumen-lines))
             (lumen-debug-message (format "Lumen processing line: %s" line))
             (lumen-parse-output line)))))

(defun lumen-out-cb (channel msg)
  "Callback for standard output."
  (lumen-debug-message (format "Lumen out cb: %s" msg))
  (lumen-parse-output msg))

(defun lumen-on-stderr (process output)
  "Handle standard error from job."
  (let ((data (split-string output "\n" t)))
    (lumen-debug-message (format "Lumen on stderr: %s" output))
    (setq lumen-elines (append lumen-elines data))
    (while (> (length lumen-elines) 1)
           (let ((line (car lumen-elines)))
             (setq lumen-elines (cdr lumen-elines))
             (lumen-debug-log-err line)))))

(defun lumen-err-cb (channel msg)
  "Callback for standard error."
  (lumen-debug-message (format "Lumen err cb: %s" msg))
  (lumen-debug-log-err msg))

(defun lumen-on-exit (process status)
  "Handle job exit."
  (let ((code (process-exit-status process)))
    (lumen-debug-message (format "Lumen on exit: Job %s exited with code %d" process code))
    (setq lumen-exit-code code)))

(defun lumen-exit-cb (job code)
  "Callback for job exit."
  (lumen-debug-message (format "Lumen exit cb: Job %s exited with code %d" job code))
  (setq lumen-exit-code code))

(defun lumen-fork-job ()
  "Fork a job to run the command."
  (remove-hook 'after-init-hook 'lumen-fork-job)
  (lumen-debug-message "Lumen fork job: Removing hook and starting job...")
  (let ((command (lumen-platforms-linux-watch-cmd)))
    (lumen-debug-message (format "Lumen fork job: Command is %s" (string-join command " ")))
    (when command
      (let ((proc (apply #'start-process "lumen-job" nil command)))
        (set-process-filter proc #'lumen-on-stdout)
        (set-process-sentinel proc #'lumen-on-exit)))))

(defun lumen-job-state ()
  "Get current job state."
  (let ((res ""))
    (let ((pid (when (< lumen-exit-code 0) (process-id (get-process "lumen-job")))))
      (setq res (if pid (format "run as PID %d" pid) "dead")))
    (when (> lumen-exit-code -1)
      (setq res (concat res (format " (exit code %d)" lumen-exit-code))))
    (lumen-debug-message (format "Lumen job state: %s" res))
    res))

(defun lumen-platforms-linux-watch-cmd ()
  "Command to listens for interrupts and/or signals from D-Bus on Linux platforms."
  (let ((gdbus-path (executable-find "gdbus")))  ;; Find the gdbus executable
    (if gdbus-path
      (list gdbus-path
            "monitor"
            "--session"
            "--dest" "org.freedesktop.portal.Desktop"
            "--object-path" "/org/freedesktop/portal/desktop")
      (error "gdbus not found in PATH"))))

(defun lumen-platforms-linux-parse-line (line)
  "Parse line output from D-Bus."
  (lumen-debug-message (format "Lumen platforms linux parse line: %s" line))
  (when (string-prefix-p lumen-watched-line line)
    (let ((val (- (aref line (- (length line) 3)) 48)))
      (lumen-debug-message (format "Lumen parsed value: %d" val))
      (cond ((or (= val 2) (= val 0))
             (lumen-light-hook))
            ((= val 1) (lumen-dark-hook))))))

(defun lumen-debug-log-err (msg)
  "Log an error message."
  (lumen-debug-message (format "Lumen error: %s" msg)))

(defun lumen-init ()
  "Initialize Lumen configuration."
  (lumen-debug-message "Lumen init: Starting...")
  (unless (bound-and-true-p lumen-startup-overwrite)
    (setq lumen-startup-overwrite t))
  (lumen-debug-message (format "Lumen init: startup-overwrite set to %s" lumen-startup-overwrite))
  (when lumen-startup-overwrite
    (lumen-oneshot))
  (add-hook 'after-init-hook 'lumen-fork-job)
  (lumen-debug-message "Lumen init: Hook added."))

(lumen-init)

(provide 'core-lumen)

