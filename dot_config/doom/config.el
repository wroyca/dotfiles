;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi-tinted)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Make which-key popup faster. Restore defaults once we're accustomed to
;; doomemacs keybinds.
;;
;; https://github.com/doomemacs/doomemacs/issues/1839

(setq which-key-idle-delay 0.3)
(which-key-mode)

;; Make treemacs selectable by other-window (e.g. to moves between Vim
;; viewports).

(setq treemacs-is-never-other-window nil)
(after! treemacs
  (treemacs-follow-mode 1))

;; Use clangd as doomemacs c/c++ LSP server and remove the artificial delay in
;; completion.

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; ccls was previously by default.
(after! lsp-clients
  (set-lsp-priority! 'clangd 1))

(setq lsp-idle-delay 0.1
      company-idle-delay 0.0
      company-minimum-prefix-length 1)

;; Configure clangd defaults.
(setq lsp-clients-clangd-args '("--all-scopes-completion=true"
                                "--background-index=true"
                                "--background-index-priority=normal"
                                "--clang-tidy"
                                "--completion-parse=always"
                                "--completion-style=bundled"
                                "--function-arg-placeholders=false"
                                "--header-insertion=never"
                                "--parse-forwarding-functions"
                                "--pch-storage=memory"
                                "--ranking-model=decision_forest"))

;; Emulate https://github.com/ggandor/leap.nvim behavior. Note that S s might
;; have different meanings in other major modes. We should detect this and
;; redirect calls accordingly to prevent overriding the mapping with Avy.
;; Currently, I'm only aware of Magit having such conflicts, but there may be
;; others.

(defun avy-goto-char-2-below-hook ()
  (interactive)
  (if (eq major-mode 'magit-status-mode)
      (call-interactively 'magit-stage)
    (call-interactively 'avy-goto-char-2-below)))

(defun avy-goto-char-2-above-hook()
  (interactive)
  (call-interactively 'avy-goto-char-2-above))

(map! :map 'override :nv "s" #'avy-goto-char-2-below-hook)
(map! :map 'override :nv "S" #'avy-goto-char-2-above-hook)

(use-package! avy
  :config
  (setq avy-all-windows nil)
  (setq avy-background nil)
  (setq avy-highlight-first nil)
  (setq avy-single-candidate-jump t))

;; Emulate https://github.com/echasnovski/mini.operators multiply (duplicate)
;; text behavior.

(defun duplicate-text ()
  "Duplicate text"
  (interactive)
  (let ((column (- (point) (line-beginning-position)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(map! :map 'override :nv "gm" #'duplicate-line)

;; Instructs Emacs to listen to changes to the OS's color scheme and switch to a
;; matching theme accordingly.

(after! doom-ui
  (setq! auto-dark-dark-theme  'modus-vivendi-tinted
         auto-dark-light-theme 'modus-operandi-tinted)
  (auto-dark-mode 1))

;; Instructs Emacs to switch terminal colors to matching theme accordingly. Note
;; that this is assuming OSC 10 and 11 conformance.
;;
;; BUG: advice-add is deprecated, but alternatives will freeze doomemacs.
;; BUG: Emacs colorschemes frequently break in terminal mode, leading to invalid
;; states that disrupt our hook until a restart is performed.

(defvar after-enable-theme-hook nil)

(add-hook 'after-enable-theme-hook
          (lambda () (send-string-to-terminal (format "\e]11%s\a" (frame-parameter nil 'background-color)))))
(add-hook 'kill-emacs-hook
          (lambda () (send-string-to-terminal "\e]111;;\a")))

(defun run-after-enable-theme-hook (&rest _args)
  (run-hooks 'after-enable-theme-hook))
(advice-add 'enable-theme :after #'run-after-enable-theme-hook)
