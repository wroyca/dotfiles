;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(load (setopt custom-file (expand-file-name "custom.el" user-emacs-directory)))

;;

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;

(elpaca (leaf))
(elpaca (leaf-keywords)
  (leaf-keywords-init))

;;

(elpaca-wait)

;;

;; NOTE:
;;
;; In nested `leaf' declarations, child packages are configured as
;; part of the parent’s `:config' block. If a child package is
;; processed before the actual package it depends on has been
;; installed, this can result in errors or incomplete setup.
;;
;; To work around this, we introduce an intermediate dummy block. The
;; dummy serves two purposes:
;;
;; 1. It defers configuration of real packages until their
;;    dependencies are available.
;;
;; 2. It groups related packages cleanly without tying their
;;    configuration directly to the logical parent.

(leaf +interface
  :config
  (leaf +input
    :config
    ;; Configured in lisp/dotemacs-meow.el
    (leaf meow
      :elpaca t
      :global-minor-mode meow-global
      :setq (meow-clipboard-save . t))
  (leaf +feedback
    :config
    (leaf which-key
      :elpaca t
      :global-minor-mode t
      :setq (which-key-max-description-length . 40)))))

(leaf +editing
  :config
  (leaf +text
    :config
    (leaf xclip
      :elpaca t
      :global-minor-mode t)))

(leaf +completion
  :config
  (leaf +engine
    :config
    (leaf +vertico
      :config
      (leaf vertico
        :elpaca t
        :global-minor-mode t)
      (leaf vertico-mouse
        :hook (vertico-mode-hook . vertico-mouse-mode))))
  (leaf +interface
    :config
    (leaf marginalia
      :elpaca t
      :global-minor-mode t)))

(leaf +syntax
  :config
  (leaf +checking
    :config
    (leaf flymake
      :elpaca t
      :global-minor-mode t)))

(leaf +language-services
  :config
  (leaf +lsp
    :config
    (leaf +client
      :config
      (leaf +eglot
        :config
        (leaf eglot
          :elpaca t
          :defvar eglot-server-programs
          :defer-config
          (add-to-list 'eglot-server-programs
            '(c++-mode . ("clangd"
                          "--all-scopes-completion=true"
                          "--background-index=true"
                          "--background-index-priority=normal"
                          "--clang-tidy=true"
                          "--completion-parse=always"
                          "--ranking-model=decision_forest"
                          "--completion-style=bundled"
                          "--fallback-style=GNU"
                          "--function-arg-placeholders=0"
                          "--header-insertion=never"
                          "--pch-storage=memory"
                          "--parse-forwarding-functions")))
          :hook
          ((prog-mode-hook) . eglot-ensure))
        (leaf eglot-inactive-regions
          :elpaca t
          :require t
          :global-minor-mode t
          :custom (eglot-inactive-regions-style . 'darken-foreground)
                  (eglot-inactive-regions-opacity . 0.4))
    (leaf +completion
      :config
      (leaf +corfu
        :config
        (leaf corfu
          :elpaca t
          :global-minor-mode global-corfu-mode
          :custom ((corfu-auto . t)
                   (corfu-auto-delay . 0)
                   (corfu-auto-prefix . 1)))
        (leaf corfu-terminal
          :elpaca t
          :global-minor-mode t)))))))

(leaf +vcs
  :config
  (leaf +git
    :config
    (leaf +magit
      :config
      (leaf magit
        :elpaca t)
      (leaf forge
        :elpaca t)
      (leaf transient
        :elpaca t))))

;;

(elpaca-wait)

;;

(load-file (expand-file-name "lisp/dotemacs-mouse.el" user-emacs-directory))
(load-file (expand-file-name "lisp/dotemacs-meow.el" user-emacs-directory))

;;

(global-dotemacs-mouse-selection-mode 1)

(require 'dotemacs-meow)

(dotemacs-meow-setup)

;;

(provide 'init)

;;; init.el ends here
