;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotemacs//tty-setup-hook (&rest _args)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message (user-login-name))
  (put 'inhibit-startup-echo-area-message 'saved-value t)
  ;; Suppress the *scratch* buffer short message
  (setq initial-scratch-message nil))

(add-hook 'tty-setup-hook 'dotemacs//tty-setup-hook)

;; Note: `tty-setup-hook' does not run while redisplay is
;; inhibited. It will trigger only after we lift the inhibition, which
;; is "too late" to hide the elements below.
(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode scroll-bar-mode tool-bar-mode))

(defun dotemacs//disable-themes (&rest _args)
  (mapc #'disable-theme custom-enabled-themes))

(advice-add #'load-theme :before #'dotemacs//disable-themes)

(load-theme 'modus-vivendi-tinted)

(xterm-mouse-mode)

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

(use-package doom-modeline
  :ensure (:wait t)
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon nil)
  :config
  ;; Re-enable redisplay
  (setq inhibit-redisplay nil)
  (redisplay t)) ;; Force immediate redisplay

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

  ;; In the Emacs’s world, the current tendency is to have the
  ;; completion logic provided by completion-at-point-functions (CAPF)
  ;; implementations. [Among the other things, this is what the
  ;; popular packages that support language server protocol (LSP) also
  ;; rely on.]
  ;;
  ;; Since company-capf works as a bridge to the standard CAPF
  ;; facility, it is probably the most often used and recommended
  ;; backend nowadays, including for Emacs Lisp coding.
  ;;
  ;; To illustrate, the following minimal backends setup already cover
  ;; a large number of basic use cases, especially so in major modes
  ;; that have CAPF support implemented.
  (company-backends '(company-capf))

  ;; Collect candidates from the buffers with the same major mode.
  (company-dabbrev-other-buffers t)

  (global-company-mode 1))
