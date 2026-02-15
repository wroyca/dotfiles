;;; custom.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(custom-set-variables
 ;; File system and Version Control.
 ;;
 ;; We rely on git for version control. Having Emacs spray `~` and `#`
 ;; files all over the source tree is just noise that confuses file
 ;; watchers and `grep`. Kill them.
 ;;
 '(make-backup-files nil)
 '(auto-save-default nil)
 '(delete-auto-save-files t)

 ;; Lockfiles (`.#`) cause more trouble than they are worth, especially
 ;; with some aggressive file sync tools or when the editor crashes and
 ;; leaves them behind. We are the only user; disable them to avoid the
 ;; stale lock prompts.
 ;;
 '(create-lockfiles nil)

 ;; Safety net: when we delete via Dired, move to trash rather than
 ;; unlinking immediately. We've shot ourselves in the foot enough times
 ;; to need this.
 ;;
 '(delete-by-moving-to-trash t)

 ;; Always resolve symlinks to the real path. Otherwise, we end up
 ;; visiting the same file under two names, and `find-file` gets confused
 ;; about which buffer is which. Plus, compilation commands need the
 ;; real path to resolve relative includes correctly.
 ;;
 '(find-file-visit-truename t)
 '(vc-follow-symlinks t)

 ;; UI and Frame Geometry.
 ;;
 ;; Tiling window managers (and even some stacking ones) struggle when
 ;; the application tries to resize itself based on font metrics or
 ;; character cells. We need Emacs to be obedient and fill the tile it
 ;; is given, pixel-perfect. Disable implied resizing.
 ;;
 '(frame-inhibit-implied-resize t)
 '(frame-resize-pixelwise t)

 ;; Skip the GNU splash; we know what editor we are using. Also, keep
 ;; the scratch buffer empty so we can paste/eval immediately without
 ;; having to clear the default text first.
 ;;
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)

 ;; We are keyboard-centric. Audible bells break flow, and dialog boxes
 ;; force a context switch to the mouse (or weird keyboard navigation).
 ;; Suppress both to keep the focus in the buffer.
 ;;
 '(ring-bell-function 'ignore)
 '(use-dialog-box nil)

 ;; Assume themes are safe. We wrote the config, so we trust the theme
 ;; we are loading.
 ;;
 '(custom-safe-themes t)

 ;; Minibuffer and Completion.
 ;;
 ;; Recursive minibuffers are essential. Often we are in the middle of
 ;; a `find-file` and need to check a variable or run a quick command
 ;; (M-:) without canceling the current prompt.
 ;;
 '(enable-recursive-minibuffers t)

 ;; Keep the prompt distinct from the input area. We make it cursor-intangible
 ;; to prevent accidentally backspacing into the prompt text, which is annoying
 ;; to fix.
 ;;
 '(minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

 ;; Filter M-x noise. There are too many internal symbols that match
 ;; common prefixes.
 ;;
 '(read-extended-command-predicate #'command-completion-default-include-p)

 ;; Formatting and Indentation.
 ;;
 ;; Tabs are display-dependent and therefore evil. We want the code to look the
 ;; same in the editor, on GitHub, and in `less`. Force spaces.
 ;;
 '(indent-tabs-mode nil)
 '(tab-width 2)

 ;; Modern typography conventions suggest a single space after a period.
 ;; Double-spacing is a typewriter artifact we don't need.
 ;;
 ;; @@: may re-enable if we decide to time-travel back to 1997 and impress
 ;; people in email
 ;;
 '(sentence-end-double-space nil)

 ;; Navigation.
 ;;
 ;; Default scrolling is jumpy (recentering the screen). We want smooth
 ;; scrolling when we hit the edge, but we need to keep a bit of margin
 ;; (4 lines) so we can see the context of where we are moving.
 ;;
 '(scroll-conservatively 101)
 '(scroll-margin 4)

 ;; Performance.
 ;;
 ;; We often inspect massive log files or build artifacts. The warning
 ;; just adds an extra keystroke to the workflow. We know the file is
 ;; big; open it anyway.
 ;;
 '(large-file-warning-threshold nil)

 ;; Visuals.
 ;;
 ;; We need a high-contrast theme that remains legible during long
 ;; coding sessions. Modus Vivendi fits the bill.
 ;;
 '(custom-enabled-themes '(modus-vivendi)))

;; Hooks.
;;
;; Apply the intangibility property set in the variables above. We
;; have to hook this in because the variable setting alone just defines
;; the property list, it doesn't activate the minor mode.
;;
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
