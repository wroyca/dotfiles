;; [[file:README.org::*Initialization][Initialization:1]]
;; init.el --- -*- lexical-binding: t -*-
;; Initialization:1 ends here

;; [[file:README.org::*Elpaca][Elpaca:1]]
(defvar elpaca-installer-version 0.8)
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca leaf)
(elpaca leaf-keywords
  (leaf-keywords-init))
(elpaca leaf-tree)
(elpaca leaf-convert)
(elpaca-wait)
;; Elpaca:1 ends here

;; [[file:README.org::*Meow][Meow:1]]
(leaf meow
      :doc "Yet another modal editing on Emacs / 猫态编辑"
      :url "https://github.com/meow-edit/meow"
      :elpaca t
      :require (meow-helpers
                 meow-cheatsheet)
      :global-minor-mode meow-global
      :init
      (defun dotemacs//meow-setup ()
        (with-eval-after-load 'meow-cheatsheet
                              (setq meow-cheatsheet-layout
                                    meow-cheatsheet-layout-colemak-dh))
        (meow-leader-define-key
          '("1" . meow-digit-argument)
          '("2" . meow-digit-argument)
          '("3" . meow-digit-argument)
          '("4" . meow-digit-argument)
          '("5" . meow-digit-argument)
          '("6" . meow-digit-argument)
          '("7" . meow-digit-argument)
          '("8" . meow-digit-argument)
          '("9" . meow-digit-argument)
          '("0" . meow-digit-argument)
          '("/" . meow-keypad-describe-key)
           '("?" . meow-cheatsheet))
        (meow-motion-overwrite-define-key
          '("<escape>" . ignore))
        (meow-normal-define-key
          '("0" . meow-expand-0)
          '("9" . meow-expand-9)
          '("8" . meow-expand-8)
          '("7" . meow-expand-7)
          '("6" . meow-expand-6)
          '("5" . meow-expand-5)
          '("4" . meow-expand-4)
          '("3" . meow-expand-3)
          '("2" . meow-expand-2)
          '("1" . meow-expand-1)
          '("-" . negative-argument)
          '(";" . meow-reverse)
          '("," . meow-inner-of-thing)
          '("." . meow-bounds-of-thing)
          '("<" . meow-beginning-of-thing)
          '(">" . meow-end-of-thing)
          '("a" . meow-append)
          '("A" . meow-open-below)
          '("b" . meow-back-word)
          '("B" . meow-back-symbol)
          '("c" . meow-change)
          '("d" . meow-delete)
          '("D" . meow-backward-delete)
          '("e" . meow-line)
          '("E" . meow-goto-line)
          '("f" . meow-find)
          '("g" . meow-cancel-selection)
          '("G" . meow-grab)
          '("h" . meow-left)
          '("H" . meow-left-expand)
          '("i" . meow-insert)
          '("I" . meow-open-above)
          '("j" . meow-join)
          '("k" . meow-kill)
          '("l" . meow-till)
          '("m" . meow-mark-word)
          '("M" . meow-mark-symbol)
          '("n" . meow-next)
          '("N" . meow-next-expand)
          '("o" . meow-block)
          '("O" . meow-to-block)
          '("p" . meow-prev)
          '("P" . meow-prev-expand)
          '("q" . meow-quit)
          '("Q" . meow-goto-line)
          '("r" . meow-replace)
          '("R" . meow-swap-grab)
          '("s" . meow-search)
          '("t" . meow-right)
          '("T" . meow-right-expand)
          '("u" . meow-undo)
          '("U" . meow-undo-in-selection)
          '("v" . meow-visit)
          '("w" . meow-next-word)
          '("W" . meow-next-symbol)
          '("x" . meow-save)
          '("X" . meow-sync-grab)
          '("y" . meow-yank)
          '("z" . meow-pop-selection)
          '("'" . repeat)
          '("<escape>" . ignore)))
      :config
      (dotemacs//meow-setup))
;; Meow:1 ends here

;; [[file:README.org::*Vertico][Vertico:1]]
(leaf *vertico
      :config
      (leaf vertico
            :doc "VERTical Interactive COmpletion"
            :url "https://github.com/minad/vertico"
            :elpaca t
            :global-minor-mode t)
      (leaf vertico-buffer
            :doc "Display Vertico like a regular buffer."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el"
            :after vertico)
      (leaf vertico-directory
            :doc "Commands for Ido-like directory navigation."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el"
            :after vertico
            :bind (:vertico-map :package vertico
                                ("RET"   . vertico-directory-enter)
                                ("DEL"   . vertico-directory-delete-char)
                                ("M-DEL" . vertico-directory-delete-word)))
      (leaf vertico-flat
            :doc "Enable a flat, horizontal display."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-flat.el"
            :after vertico)
      (leaf vertico-grid
            :doc "Enable a grid display."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-grid.el"
            :after vertico)
      (leaf vertico-indexed
            :doc "Select indexed candidates with prefix arguments."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-indexed.el"
            :after vertico)
      (leaf vertico-mouse
            :doc "Support mouse for scrolling and candidate selection."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-mouse.el"
            :after vertico
            :hook
            (vertico-mode-hook . vertico-mouse-mode))
      (leaf vertico-multiform
            :doc "Configure Vertico modes per command or completion category."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-multiform.el"
            :after vertico)
      (leaf vertico-quick
            :doc "Commands to select using Avy-style quick keys."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el"
            :after vertico)
      (leaf vertico-repeat
            :doc "Repeats the last completion session."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el"
            :after vertico)
      (leaf vertico-reverse
            :doc "Reverse the display."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-reverse.el"
            :after vertico)
      (leaf vertico-suspend
            :doc "Suspends and restores the current session."
            :url "https://github.com/minad/vertico/blob/main/extensions/vertico-suspend.el"
            :after vertico)
      )
;; Vertico:1 ends here

;; [[file:README.org::*Marginalia][Marginalia:1]]
(leaf marginalia
      :doc "Marginalia in the minibuffer"
      :url "https://github.com/minad/marginalia"
      :elpaca t
      :global-minor-mode t)
;; Marginalia:1 ends here

;; [[file:README.org::*Consult][Consult:1]]
(leaf consult
      :doc "consult.el - Consulting completing-read"
      :url "https://github.com/minad/consult"
      :elpaca t
      :disabled nil) ;; Consult is recommended. Learn about it later.
;; Consult:1 ends here

;; [[file:README.org::*Embark][Embark:1]]
(leaf embark
      :doc "Emacs Mini-Buffer Actions Rooted in Keymaps"
      :url "https://github.com/oantolin/embark"
      :elpaca t
      :disabled t) ;; Embark is recommended. Learn about it later.
;; Embark:1 ends here

;; [[file:README.org::*Orderless][Orderless:1]]
(leaf orderless
      :doc "Emacs completion style that matches multiple regexps in any order."
      :url "https://github.com/oantolin/orderless"
      :elpaca t
      :custom ((completion-styles . '(orderless basic))
               (completion-category-defaults . nil)
               (completion-category-overrides '((file (styles partial-completion))))))
;; Orderless:1 ends here

;; [[file:README.org::*Magit][Magit:1]]
(leaf *magit
      :config
      (leaf magit
            :doc "It's Magit! A Git porcelain inside Emacs."
            :url "https://github.com/magit/magit"
            :elpaca t)
      (leaf magit-delta
            :doc "Use delta (https://github.com/dandavison/delta) when viewing diffs in Magit "
            :url "https://github.com/dandavison/magit-delta"
            :elpaca t
            :hook
            (magit-mode-hook . magit-delta-mode))
      (leaf forge
            :doc "Work with Git forges from the comfort of Magit"
            :url "https://github.com/magit/forge"
            :elpaca t
            :setq (auth-sources '("~/.authinfo")))
      ;; https://github.com/progfolio/elpaca/issues/272
      (leaf transient
            :doc "Transient commands"
            :url "https://github.com/magit/transient"
            :elpaca t))
;; Magit:1 ends here

;; [[file:README.org::*Dimmer][Dimmer:1]]
(leaf dimmer
      :doc "Interactively highlight which buffer is active by dimming the others."
      :url "https://github.com/gonewest818/dimmer.el"
      :elpaca t
      :global-minor-mode t
      :custom
      (dimmer-prevent-dimming-predicates . '(window-minibuffer-p))
      (dimmer-fraction . 0.5)
      (dimmer-adjustment-mode . :foreground)
      (dimmer-use-colorspace . :rgb)
      (dimmer-watch-frame-focus-events . nil) ; don't dim buffers when Emacs loses focus
      ((lambda ()
         "Exclude Vertico buffer from dimming."
         (with-no-warnings
           (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*Vertico\\*$")))))
;; Dimmer:1 ends here

;; [[file:README.org::*Org][Org:1]]
(leaf *org
      :config
      (leaf org
            :doc "Fast and effective plain text system."
            :url "https://orgmode.org/"
            :elpaca t
            :setq ((org-auto-align-tags                . nil)
                   (org-tags-column                    . 0)
                   (org-catch-invisible-edits          . 'show-and-error)
                   (org-special-ctrl-a/e               . t)
                   (org-insert-heading-respect-content . t)
                   (org-hide-emphasis-markers          . t)
                   (org-pretty-entities                . t)
                   (org-ellipsis                       . "…")))
      (leaf org-modern
            :doc "Modern Org Style"
            :url "https://github.com/minad/org-modern"
            :elpaca t
            :hook
            (org-mode-hook . org-modern-mode)))
;; Org:1 ends here

;; [[file:README.org::*Clipboard][Clipboard:1]]
(leaf xclip
      :elpaca t
      :global-minor-mode t)
;; Clipboard:1 ends here

;; [[file:README.org::*Language Server Protocol][Language Server Protocol:1]]
(leaf *language-server-protocol
      :config
      (leaf eglot
            :doc "a client for language server protocol servers"
            :url "https://github.com/joaotavora/eglot"
            :elpaca t
            :defvar eglot-server-programs
            :defer-config
            (add-to-list 'eglot-server-programs
                         '(c++-mode . ("clangd"
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
            ((c++-mode-hook) . eglot-ensure))
      
      (leaf company
            :doc "Modular text completion framework"
            :url "http://company-mode.github.io/"
            :elpaca t
            :leaf-defer nil
            :bind ((company-active-map
                     ("[tab]"    . company-complete-selection)
                     ("TAB"      . company-complete-selection)
                     ("<return>" . nil)
                     ("RET"      . nil)))
            :custom ((company-dabbrev-other-buffers . t)
                     (company-format-margin-function . nil)
                     (company-idle-delay . 0)
                     (company-minimum-prefix-length . 1)
                     (company-tooltip-align-annotations . t)
                     (company-tooltip-limit . 8))
            :global-minor-mode global-company-mode))
;; Language Server Protocol:1 ends here

;; [[file:README.org::*Built-in packages][Built-in packages:1]]
(leaf *built-in
      :config
      (leaf savehist
            :doc "Save minibuffer history"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el"
            :global-minor-mode t)
      (leaf save-place
            :doc "Automatically save place in files"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/saveplace.el"
            :global-minor-mode t)
      (leaf recentf
            :disabled t
            :doc "Keep track of recently opened files"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/recentf.el"
            :global-minor-mode t)
      (leaf auto-revert
            :doc "Revert buffers when files on disk change "
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/autorevert.el"
            :global-minor-mode global-auto-revert)
      (leaf winner
            :doc "Restore old window configurations"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/winner.el"
            :global-minor-mode t)
      (leaf cua
            :doc "CUA mode for copy-paste conventions"
            :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/CUA-Bindings.html"
            :custom ((cua-keep-region-after-copy . t))
            :global-minor-mode t)
      (leaf context-menu
            :doc "Toggle context menu"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/mouse.el"
            :global-minor-mode t)
      (leaf editorconfig
            :doc "EditorConfig support"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/editorconfig-core.el"
            :global-minor-mode editorconfig-mode)
      (leaf xterm-mouse
            :doc "support the mouse when emacs run in an xterm"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/xt-mouse.el"
            :global-minor-mode xterm-mouse
            :custom ((scroll-conservatively . 101)
                     (scroll-margin . 4)
                     (mouse-wheel-scroll-amount
                       . '(3 ((shift) . 5) ((control) . nil)))
                     (mouse-wheel-progressive-speed . nil)))
      (leaf compilation-shell-minor
            :doc "Compilation shell minor mode"
            :url "https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/compile.el"
            :hook ((compilation-mode . compilation-shell-minor-mode))))
;; Built-in packages:1 ends here
