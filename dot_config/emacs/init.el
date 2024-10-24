;;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotemacs//disable-themes (&rest _args)
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'dotemacs//disable-themes)

(defun dotemacs//xterm-osc10 (&rest _args)
  (send-string-to-terminal (format "\e]11;%s\a" (frame-parameter nil 'background-color))))
(advice-add #'load-theme :after #'dotemacs//xterm-osc10)

(defun dotemacs//xterm-osc11 (&rest _args)
  (send-string-to-terminal "\e]111;\a"))
(add-hook 'kill-emacs-hook #'dotemacs//xterm-osc11)

(when (getenv "WAYLAND_DISPLAY")
  (setq wl-copy-p nil
        interprogram-cut-function (lambda (text)
                                    (setq-local process-connection-type 'pipe)
                                    (setq wl-copy-p (start-process "wl-copy" nil "wl-copy" "-f" "-n"))
                                    (process-send-string wl-copy-p text)
                                    (process-send-eof wl-copy-p))
        interprogram-paste-function (lambda ()
                                      (unless (and wl-copy-p (process-live-p wl-copy-p))
                                        (shell-command-to-string "wl-paste -n | tr -d '\r'")))))
                                        
(setq make-backup-files nil)
(xterm-mouse-mode)
(load-theme 'modus-vivendi-tinted)

;;

(use-package emacs
  :ensure nil
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd"
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
  (sh-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

(defun my-suppress-eglot-message (orig-fun format &rest args)
  "Suppress specific eglot messages from being shown in the minibuffer."
  (let ((message-string (apply #'format format args)))
    (unless (or (string-prefix-p "Connected" message-string)
                (string-prefix-p "Waiting" message-string)
                (string-prefix-p "Reconnected" message-string))
      (apply orig-fun format args))))

(advice-add 'eglot--message :around #'my-suppress-eglot-message)

;;

(use-package clangd-inactive-regions
  :ensure (:host github :repo "fargiolas/clangd-inactive-regions.el")
  :init
  (add-hook 'eglot-managed-mode-hook #'clangd-inactive-regions-mode)
  :config
  (clangd-inactive-regions-set-method "darken-foreground")
  (clangd-inactive-regions-set-opacity 0.50))

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
  (elpaca-use-package-mode))

(use-package meow
 :ensure t
 :demand t
 :config
 (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
 (meow-motion-overwrite-define-key
  ;; Use e to move up, n to move down.
  ;; Since special modes usually use n to move down, we only overwrite e here.
  '("e" . meow-prev)
  '("<escape>" . ignore))
 (meow-leader-define-key
  '("?" . meow-cheatsheet)
  ;; To execute the originally e in MOTION state, use SPC e.
  '("e" . "H-e")
  '("1" . meow-digit-argument)
  '("2" . meow-digit-argument)
  '("3" . meow-digit-argument)
  '("4" . meow-digit-argument)
  '("5" . meow-digit-argument)
  '("6" . meow-digit-argument)
  '("7" . meow-digit-argument)
  '("8" . meow-digit-argument)
  '("9" . meow-digit-argument)
  '("0" . meow-digit-argument))
 (meow-normal-define-key
  '("0" . meow-expand-0)
  '("1" . meow-expand-1)
  '("2" . meow-expand-2)
  '("3" . meow-expand-3)
  '("4" . meow-expand-4)
  '("5" . meow-expand-5)
  '("6" . meow-expand-6)
  '("7" . meow-expand-7)
  '("8" . meow-expand-8)
  '("9" . meow-expand-9)
  '("-" . negative-argument)
  '(";" . meow-reverse)
  '("," . meow-inner-of-thing)
  '("." . meow-bounds-of-thing)
  '("[" . meow-beginning-of-thing)
  '("]" . meow-end-of-thing)
  '("/" . meow-visit)
  '("a" . meow-append)
  '("A" . meow-open-below)
  '("b" . meow-back-word)
  '("B" . meow-back-symbol)
  '("c" . meow-change)
  '("e" . meow-prev)
  '("E" . meow-prev-expand)
  '("f" . meow-find)
  '("g" . meow-cancel-selection)
  '("G" . meow-grab)
  '("h" . meow-left)
  '("H" . meow-left-expand)
  '("i" . meow-right)
  '("I" . meow-right-expand)
  '("j" . meow-join)
  '("k" . meow-kill)
  '("l" . meow-line)
  '("L" . meow-goto-line)
  '("m" . meow-mark-word)
  '("M" . meow-mark-symbol)
  '("n" . meow-next)
  '("N" . meow-next-expand)
  '("o" . meow-block)
  '("O" . meow-to-block)
  '("p" . meow-yank)
  '("q" . meow-quit)
  '("r" . meow-replace)
  '("s" . meow-insert)
  '("S" . meow-open-above)
  '("t" . meow-till)
  '("u" . meow-undo)
  '("U" . meow-undo-in-selection)
  '("v" . meow-search)
  '("w" . meow-next-word)
  '("W" . meow-next-symbol)
  '("x" . meow-delete)
  '("X" . meow-backward-delete)
  '("y" . meow-save)
  '("z" . meow-pop-selection)
  '("'" . repeat)
  '("<escape>" . ignore))
 ;; Meow!
 (meow-global-mode 1))

(use-package vertico
  :ensure t
  :hook
  (elpaca-after-init . vertico-mode)
  (vertico-mode . vertico-mouse-mode))

(use-package vertico-buffer
  :after vertico
  :ensure nil)

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-flat
  :after vertico
  :ensure nil)

(use-package vertico-grid
  :after vertico
  :ensure nil)

(use-package vertico-indexed
  :after vertico
  :ensure nil)

(use-package vertico-mouse
  :after vertico
  :ensure nil)

(use-package vertico-multiform
  :after vertico
  :ensure nil)

(use-package vertico-quick
  :after vertico
  :ensure nil)

(use-package vertico-repeat
  :after vertico
  :ensure nil)

(use-package vertico-reverse
  :after vertico
  :ensure nil)

(use-package vertico-suspend
  :after vertico
  :ensure nil)

(use-package vertico-unobtrusive
  :after vertico
  :ensure nil)

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
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :custom
  (list `(,(expand-file-name "~/Projects/") . 1))
  :hook
  ;; Automatically refresh Magit buffers
  ;; NOTE: Can lead to a noticeable delay in big repositories.
  (after-save . magit-after-save-refresh-status)
  :config
  (magit-save-repository-buffers 'dontask))

(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package forge
  :ensure t
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))
