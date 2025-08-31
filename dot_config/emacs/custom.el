;;; custom.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(custom-set-variables
 ;; ---------------------------
 ;; File handling / backups
 ;; ---------------------------

 ;; Disable backup files like `file~`.
 '(make-backup-files nil)

 ;; Disable auto-save files like `#file#`.
 '(auto-save-default nil)

 ;; Remove leftover auto-save files after proper saves.
 '(delete-auto-save-files t)

 ;; Do not create lockfiles like `.#file`.
 '(create-lockfiles nil)

 ;; Use system trash when deleting files through Emacs.
 '(delete-by-moving-to-trash t)

 ;; When opening files, visit the real file (resolve symlinks).
 '(find-file-visit-truename t)

 ;; Follow VC-managed symlinks without prompting.
 '(vc-follow-symlinks t)


 ;; ---------------------------
 ;; UI / Frame / startup / scratch
 ;; ---------------------------

 ;; Treat themes as safe by default (avoid prompts).
 '(custom-safe-themes t)

 ;; Inhibit frame resizing triggered by font changes.
 '(frame-inhibit-implied-resize t)

 ;; Allow pixel-wise frame resizing (good for HiDPI / tiling).
 '(frame-resize-pixelwise t)

 ;; Do not show the startup screen.
 '(inhibit-startup-screen t)

 ;; Start with empty *scratch* message.
 '(initial-scratch-message nil)

 ;; Silence audible/visual bells.
 '(ring-bell-function 'ignore)

 ;; Do not show dialog
 '(use-dialog-box nil)


 ;; ---------------------------
 ;; Minibuffer & completion
 ;; ---------------------------

 ;; Allow recursive use of the minibuffer.
 '(enable-recursive-minibuffers t)

 ;; Make the minibuffer prompt read-only and cursor-intangible.
 '(minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

 ;; Filter what appears in `M-x` completion.
 '(read-extended-command-predicate #'command-completion-default-include-p)


 ;; ---------------------------
 ;; Editing / indentation
 ;; ---------------------------

 ;; Prefer spaces over tabs.
 '(indent-tabs-mode nil)

 ;; Modern sentence handling: single space after period.
 '(sentence-end-double-space nil)

 ;; Logical default when a literal tab appears.
 '(tab-width 2)


 ;; ---------------------------
 ;; Scrolling / navigation
 ;; ---------------------------

 ;; Conservative scrolling to avoid recentering.
 '(scroll-conservatively 101)

 ;; Keep a small margin when the point approaches the window edge.
 '(scroll-margin 4)


 ;; ---------------------------
 ;; Performance / safety
 ;; ---------------------------

 ;; Don't warn on large files
 '(large-file-warning-threshold nil)


 ;; ---------------------------
 ;; Defaults
 ;; ---------------------------

 ;; Remember the last used theme between Emacs' sessions.
 '(custom-enabled-themes '(modus-vivendi))

 ;; Set the default starting directory for new buffers and file prompts.
 ;; This expression is evaluated when this file is loaded so "~" expands.
 `(default-directory ,(expand-file-name "~/Projects/")))

;; --------------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------------
;; Make the minibuffer prompt cursor-intangible so the point cannot enter it.
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; --------------------------------------------------------------------------
;; Projects
;; --------------------------------------------------------------------------
;; Refresh known projects list so it stays in sync with the file system.
(require 'project)
(let ((project-search-dirs '("~/Projects/")))
  ;; Remove invalid or non-existent projects from the list.
  (project-forget-zombie-projects)
  ;; Remember all projects under the configured search directories.
  (dolist (dir project-search-dirs)
    (project-remember-projects-under dir t)))
