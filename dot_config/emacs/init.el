;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defvar dotemacs--inhibit-startup-screen t
  "When non-nil, inhibit the startup screen on Emacs launch.")

(defun dotemacs//inhibit-startup-screen ()
    "Set `inhibit-startup-screen' to the value of `dotemacs--inhibit-startup-screen'.
This acts as a shortcut to disable the startup screen, splash screen,
and startup message."
  (setq inhibit-startup-screen 'dotemacs--inhibit-startup-screen))
(defvar dotemacs--inhibit-startup-echo-area-message (user-login-name)
  "When non-nil, inhibit the startup echo area message on Emacs launch.")

(defun dotemacs//inhibit-startup-echo-area-message ()
  "Set `inhibit-startup-echo-area-message' to the value of `dotemacs--inhibit-startup-echo-area-message'.
This acts as a shortcut to disable the startup echo area message."
  (setq-default inhibit-startup-echo-area-message dotemacs--inhibit-startup-echo-area-message)
  ;; https://yrh.dev/blog/rant-obfuscation-in-emacs/
  (put 'inhibit-startup-echo-area-message 'saved-value t))
(defvar dotemacs--initial-scratch-message nil
  "When non-nil, suppress the initial scratch message.")

(defun dotemacs//initial-scratch-message ()
    "Set `initial-scratch-message' to the value of
`dotemacs--initial-scratch-message'.
This acts as a shortcut to override the initial scratch message."
  (setq-default initial-scratch-message dotemacs--initial-scratch-message))

(defun dotemacs//tty-setup-hook (&rest _args)
  (dotemacs//inhibit-startup-screen)
  (dotemacs//inhibit-startup-echo-area-message)
  (dotemacs//initial-scratch-message))

(add-hook 'tty-setup-hook 'dotemacs//tty-setup-hook)

(defun dotemacs//disable-themes (&rest _args)
  (mapc #'disable-theme custom-enabled-themes))

(advice-add #'load-theme :before #'dotemacs//disable-themes)

(load-theme 'modus-vivendi)

(xterm-mouse-mode)

;; TODO: Terminals handle scrolling differently (e.g., Kitty's default is
;; to scroll by 4, Ptyxis by 1, and so on). For now, set the default
;; as if using Ptyxis, but later detect the terminal properly.
(setq mouse-wheel-scroll-amount '(3 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; FIXME: Strange behavior with mouse wheel scroll when reading document end.
;; (setq scroll-margin 4)

(defun dotemacs//xterm-change-text-background (&rest _args)
  (send-string-to-terminal
   (format "\e]11;%s\a" (frame-parameter nil 'background-color))))

(advice-add #'load-theme :after #'dotemacs//xterm-change-text-background)
(add-hook 'resume-tty-functions #'dotemacs//xterm-change-text-background)

;; Run once as pre-shot routine.
(dotemacs//xterm-change-text-background)

(defun dotemacs//xterm-reset-text-background (&rest _args)
  (send-string-to-terminal "\e]111;\a"))

(add-hook 'kill-emacs-hook #'dotemacs//xterm-reset-text-background)
(add-hook 'suspend-tty-functions #'dotemacs//xterm-reset-text-background)

(defvar dotemacs--ptyxis-open-files '()
  "List of files currently opened in Ptyxis tabs.")

(defvar dotemacs--tangling nil
  "Non-nil if currently tangling with `org-babel-tangle'.")

(defun dotemacs//with-tangling-active (orig-fun &rest args)
  "Set `dotemacs--tangling` to non-nil during `org-babel-tangle`."
  (let ((dotemacs--tangling t))
    (apply orig-fun args)))

(advice-add 'org-babel-tangle :around #'dotemacs//with-tangling-active)

(defun dotemacs//ptyxis-generate-tab-command (file)
  "Generate the command to open FILE in a new Ptyxis tab with Emacs client."
  (let ((title (concat (file-name-nondirectory file) " - ")))
    (format "/home/wroy/.local/bin/ptyxis/emacs-new-tab %s %s"
            (shell-quote-argument title)
            (shell-quote-argument file))))

(defun dotemacs/ptyxis-open-file-in-tab (file)
  "Open FILE in a new Ptyxis tab and launch new Emacs client.
The file is also added to `dotemacs--ptyxis-open-files' for reopening
purposes."
  (interactive "FFile: ")
  (let ((cmd (dotemacs//ptyxis-generate-tab-command file)))
    (start-process-shell-command "ptyxis-open-file" nil cmd)
    (add-to-list 'dotemacs--ptyxis-open-files file)))

(defun dotemacs/ptyxis-reopen-tabs ()
  "Reopen all files listed in `dotemacs--ptyxis-open-files' in new Ptyxis tabs.
Use this function if a GTK crash occurs or tabs need to be restored."
  (interactive)
  (dolist (file dotemacs--ptyxis-open-files)
    (dotemacs/ptyxis-open-file-in-tab file)))

(defun dotemacs//ptyxis-open-file-advice (orig-fun &rest args)
  "Advice to open files in a new Ptyxis tab by default.
  ORIG-FUN is the original function, and ARGS are its arguments."
  (if dotemacs--tangling
      (apply orig-fun args)
    (let ((file (car args)))
      (if (and file (file-exists-p file) (not (file-directory-p file)))
          (dotemacs/ptyxis-open-file-in-tab file)
        (apply orig-fun args)))))

(advice-add 'find-file :around #'dotemacs//ptyxis-open-file-advice)
(advice-add 'dired-find-file :around #'dotemacs//ptyxis-open-file-advice)

(setq enable-recursive-minibuffers t)

(setq minibuffer-depth-indicate-mode t)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
		(replace-regexp-in-string
		 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		 crm-separator)
		(car args))
	(cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(savehist-mode)

(cua-mode)
(setq-default cua-keep-region-after-copy t)

(editorconfig-mode)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package vertico
  :ensure t
  :hook
  (elpaca-after-init . vertico-mode))

(use-package vertico-buffer
  :after vertico)

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-flat
  :after vertico)

(use-package vertico-grid
  :after vertico)

(use-package vertico-indexed
  :after vertico)

(use-package vertico-mouse
  :after vertico
  :hook
  (vertico-mode . vertico-mouse-mode))

(use-package vertico-multiform
  :after vertico)

(use-package vertico-quick
  :after vertico)

(use-package vertico-repeat
  :after vertico)

(use-package vertico-reverse
  :after vertico)

(use-package vertico-suspend
  :after vertico)

(use-package vertico-unobtrusive
  :after vertico)

(use-package marginalia
  :ensure t
  :hook
  (vertico-mode . marginalia-mode))

(use-package consult
  :ensure t)

(use-package embark
  :ensure t)

(use-package embark-consult
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package transient
  :ensure t)

(use-package magit
   :ensure t
   :custom
   (magit-no-message (list "Turning on magit-auto-revert-mode..."))
   (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
   :hook
   (after-save . magit-after-save-refresh-status))

(use-package magit-delta
   :ensure t
   :after magit
   :hook (magit-mode . magit-delta-mode))

(use-package forge
  :ensure t
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package org
  :ensure t)

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode))

(use-package xclip
  :ensure t
  :custom
  (xclip-mode 1))

(use-package undo-fu-session
  :ensure t
  :custom
  (undo-fu-session-global-mode 1))

(use-package eglot
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))

  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--all-scopes-completion=true"
                    "--background-index-priority=normal"
                    "--background-index=true"
                    "--clang-tidy"
                    "--completion-parse=always"
                    "--completion-style=bundled"
                    "--function-arg-placeholders=false"
                    "--header-insertion=never"
                    "--parse-forwarding-functions"
                    "--pch-storage=memory"
                    "--ranking-model=decision_forest")))

  :hook
  ((c-mode c++-mode) . eglot-ensure))

(use-package clangd-inactive-regions
  :ensure (:host github :repo "fargiolas/clangd-inactive-regions.el")
  :init
  ;; FIXME: Using `:hook' behave strangely. (lisp recursion?)
  (add-hook 'eglot-managed-mode-hook #'clangd-inactive-regions-mode)
  :config
  (clangd-inactive-regions-set-method "darken-foreground")
  (clangd-inactive-regions-set-opacity 0.55))

(use-package company
  :ensure t
  :bind (:map company-active-map
      	      ([tab] . company-complete-selection)
      	      ("TAB" . company-complete-selection)
      	      ("<return>" . nil)
      	      ("RET" . nil))
  :custom
  ;; "Tooltip" is misleading; this actually refers to the completion
  ;; menu.
  (company-tooltip-limit 8)
  (company-tooltip-align-annotations t)

  ;; Instructs company to allow typing characters that don't match any
  ;; completion candidates. When non-nil, typing characters not in the
  ;; auto-completion list is restricted.
  (company-require-match nil)

  ;; XXX: We might want to set the prefix length and idle delay based
  ;; on the language. Clangd is very fast, so it's not a concern, but
  ;; what about slower LSP clients?
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)

  ;; Disable icons.
  (company-format-margin-function nil)

  ;; Collect candidates from the buffers with the same major mode.
  (company-dabbrev-other-buffers t)

  (global-company-mode 1))

(use-package company-org-block
  :ensure t
  :custom
  (company-org-block-edit-style 'inline) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))
