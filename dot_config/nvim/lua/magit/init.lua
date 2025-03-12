---@diagnostic disable: undefined-field
---@diagnostic disable: unused-local

-- Module definition ==========================================================
local Magit = {}
local H = {}

-- Elisp templates for runtime generation ======================================

H.early_init_template = [[
;;; early-init.el --- -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(setq package-enable-at-startup nil)

(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-screen t)
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq initial-scratch-message nil)

(mapc (lambda (mode) (funcall mode -1))
  '(menu-bar-mode scroll-bar-mode tool-bar-mode))
(setq-default mode-line-format nil)

;;

(custom-set-faces
 `(magit-bisect-bad                      ((t (:background nil :foreground nil))))
 `(magit-bisect-good                     ((t (:background nil :foreground nil))))
 `(magit-bisect-skip                     ((t (:background nil :foreground nil))))
 `(magit-blame-date                      ((t (:background nil :foreground nil))))
 `(magit-blame-dimmed                    ((t (:background nil :foreground nil))))
 `(magit-blame-hash                      ((t (:background nil :foreground nil))))
 `(magit-blame-heading                   ((t (:background nil :foreground nil))))
 `(magit-blame-highlight                 ((t (:background nil :foreground nil))))
 `(magit-blame-margin                    ((t (:background nil :foreground nil))))
 `(magit-blame-name                      ((t (:background nil :foreground nil))))
 `(magit-blame-summary                   ((t (:background nil :foreground nil))))
 `(magit-branch-current                  ((t (:background nil :foreground nil))))
 `(magit-branch-local                    ((t (:background nil :foreground "p.blue"  :weight bold))))
 `(magit-branch-remote                   ((t (:background nil :foreground "p.green" :weight bold))))
 `(magit-branch-remote-head              ((t (:background nil :foreground nil))))
 `(magit-branch-upstream                 ((t (:background nil :foreground nil))))
 `(magit-branch-warning                  ((t (:background nil :foreground nil))))
 `(magit-cherry-equivalent               ((t (:background nil :foreground nil))))
 `(magit-cherry-unmatched                ((t (:background nil :foreground nil))))
 `(magit-diff-added                      ((t (:background "p.green_bg" :foreground nil))))
 `(magit-diff-added-highlight            ((t (:background "p.green_bg" :foreground nil))))
 `(magit-diff-base                       ((t (:background nil :foreground nil))))
 `(magit-diff-base-highlight             ((t (:background nil :foreground nil))))
 `(magit-diff-conflict-heading           ((t (:background nil :foreground nil))))
 `(magit-diff-context                    ((t (:background nil :foreground nil))))
 `(magit-diff-context-highlight          ((t (:background nil :foreground nil))))
 `(magit-diff-file-heading               ((t (:background nil :foreground nil))))
 `(magit-diff-file-heading-highlight     ((t (:background nil :foreground nil))))
 `(magit-diff-file-heading-selection     ((t (:background nil :foreground nil))))
 `(magit-diff-hunk-heading               ((t (:background nil :foreground nil))))
 `(magit-diff-hunk-heading-highlight     ((t (:background nil :foreground nil))))
 `(magit-diff-hunk-heading-selection     ((t (:background nil :foreground nil))))
 `(magit-diff-hunk-region                ((t (:background nil :foreground nil))))
 `(magit-diff-lines-boundary             ((t (:background nil :foreground nil))))
 `(magit-diff-lines-heading              ((t (:background nil :foreground nil))))
 `(magit-diff-our                        ((t (:background nil :foreground nil))))
 `(magit-diff-our-highlight              ((t (:background nil :foreground nil))))
 `(magit-diff-removed                    ((t (:background "p.red_bg" :foreground nil))))
 `(magit-diff-removed-highlight          ((t (:background "p.red_bg" :foreground nil))))
 `(magit-diff-revision-summary           ((t (:background nil :foreground "p.green"))))
 `(magit-diff-revision-summary-highlight ((t (:background nil :foreground nil))))
 `(magit-diff-their                      ((t (:background nil :foreground nil))))
 `(magit-diff-their-highlight            ((t (:background nil :foreground nil))))
 `(magit-diff-whitespace-warning         ((t (:background nil :foreground nil))))
 `(magit-diffstat-added                  ((t (:background nil :foreground "p.green"))))
 `(magit-diffstat-removed                ((t (:background nil :foreground "p.red"))))
 `(magit-dimmed                          ((t (:background nil :foreground "p.fg_mid2"))))
 `(magit-filename                        ((t (:background nil :foreground "p.blue"))))
 `(magit-hash                            ((t (:background nil :foreground "p.fg_mid2"))))
 `(magit-head                            ((t (:background nil :foreground nil))))
 `(magit-header-line                     ((t (:background nil :foreground nil))))
 `(magit-header-line-key                 ((t (:background nil :foreground nil))))
 `(magit-header-line-log-select          ((t (:background nil :foreground nil))))
 `(magit-keyword                         ((t (:background nil :foreground nil))))
 `(magit-keyword-squash                  ((t (:background nil :foreground nil))))
 `(magit-log-author                      ((t (:background nil :foreground "p.cyan"))))
 `(magit-log-date                        ((t (:background nil :foreground nil))))
 `(magit-log-graph                       ((t (:background nil :foreground nil))))
 `(magit-mode-line-process               ((t (:background nil :foreground nil))))
 `(magit-mode-line-process-error         ((t (:background nil :foreground nil))))
 `(magit-process-ng                      ((t (:background nil :foreground nil))))
 `(magit-process-ok                      ((t (:background nil :foreground nil))))
 `(magit-reflog-amend                    ((t (:background nil :foreground nil))))
 `(magit-reflog-checkout                 ((t (:background nil :foreground nil))))
 `(magit-reflog-cherry-pick              ((t (:background nil :foreground nil))))
 `(magit-reflog-commit                   ((t (:background nil :foreground nil))))
 `(magit-reflog-merge                    ((t (:background nil :foreground nil))))
 `(magit-reflog-other                    ((t (:background nil :foreground nil))))
 `(magit-reflog-rebase                   ((t (:background nil :foreground nil))))
 `(magit-reflog-remote                   ((t (:background nil :foreground nil))))
 `(magit-reflog-reset                    ((t (:background nil :foreground nil))))
 `(magit-refname                         ((t (:background nil :foreground nil))))
 `(magit-refname-pullreq                 ((t (:background nil :foreground nil))))
 `(magit-refname-stash                   ((t (:background nil :foreground nil))))
 `(magit-refname-wip                     ((t (:background nil :foreground nil))))
 `(magit-section-heading                 ((t (:background nil :foreground "p.blue" :weight bold))))
 `(magit-section-heading-selection       ((t (:background nil :foreground nil))))
 `(magit-section-highlight               ((t (:background nil :foreground nil :weight bold))))
 `(magit-section-secondary-heading       ((t (:background nil :foreground "p.red"))))
 `(magit-sequence-done                   ((t (:background nil :foreground nil))))
 `(magit-sequence-drop                   ((t (:background nil :foreground nil))))
 `(magit-sequence-exec                   ((t (:background nil :foreground nil))))
 `(magit-sequence-head                   ((t (:background nil :foreground nil))))
 `(magit-sequence-onto                   ((t (:background nil :foreground nil))))
 `(magit-sequence-part                   ((t (:background nil :foreground nil))))
 `(magit-sequence-pick                   ((t (:background nil :foreground nil))))
 `(magit-sequence-stop                   ((t (:background nil :foreground nil))))
 `(magit-signature-bad                   ((t (:background nil :foreground nil))))
 `(magit-signature-error                 ((t (:background nil :foreground nil))))
 `(magit-signature-expired               ((t (:background nil :foreground nil))))
 `(magit-signature-expired-key           ((t (:background nil :foreground nil))))
 `(magit-signature-good                  ((t (:background nil :foreground nil))))
 `(magit-signature-revoked               ((t (:background nil :foreground nil))))
 `(magit-signature-untrusted             ((t (:background nil :foreground nil))))
 `(magit-tag                             ((t (:background nil :foreground nil))))
 `(transient-active-infix                ((t (:background nil :foreground nil :weight bold))))
 `(transient-argument                    ((t (:background nil :foreground nil :weight bold))))
 `(transient-delimiter                   ((t (:background nil :foreground nil))))
 `(transient-disabled-suffix             ((t (:background nil :foreground nil))))
 `(transient-enabled-suffix              ((t (:background nil :foreground nil))))
 `(transient-heading                     ((t (:background nil :foreground "p.yellow" :weight bold))))
 `(transient-higher-level                ((t (:background nil :foreground nil))))
 `(transient-inactive-argument           ((t (:background nil :foreground nil))))
 `(transient-inactive-value              ((t (:background nil :foreground nil))))
 `(transient-inapt-suffix                ((t (:background nil :foreground nil))))
 `(transient-key                         ((t (:background nil :foreground "p.blue"))))
 `(transient-key-exit                    ((t (:background nil :foreground "p.blue"))))
 `(transient-key-noop                    ((t (:background nil :foreground "p.blue"))))
 `(transient-key-return                  ((t (:background nil :foreground "p.blue"))))
 `(transient-key-stay                    ((t (:background nil :foreground "p.blue"))))
 `(transient-mismatched-key              ((t (:background nil :foreground nil))))
 `(transient-nonstandard-key             ((t (:background nil :foreground nil))))
 `(transient-unreachable                 ((t (:background nil :foreground nil))))
 `(transient-unreachable-key             ((t (:background nil :foreground nil))))
 `(transient-value                       ((t (:background nil :foreground nil))))
 `(header-line                           ((t (:background nil :foreground nil))))
 `(highlight                             ((t (:background "p.bg_mid2" :foreground nil ))))
 ;`(region                                ((t (:background nil :foreground nil))))
 `(link                                  ((t (:background nil :foreground "p.blue"))))
 `(git-commit-comment-action             ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-comment-branch-local       ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-comment-branch-remote      ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-comment-detached           ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-comment-file               ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-comment-heading            ((t (:background nil :foreground "p.fg_mid2"))))
 `(git-commit-keyword                    ((t (:background nil :foreground nil))))
 `(git-commit-nonempty-second-line       ((t (:background nil :foreground nil))))
 `(git-commit-overlong-summary           ((t (:background nil :foreground nil))))
 `(git-commit-summary                    ((t (:background nil :foreground nil))))
 `(git-commit-trailer-token              ((t (:background nil :foreground nil))))
 `(git-commit-trailer-value              ((t (:background nil :foreground nil))))
 `(font-lock-comment-face                ((t (:background nil :foreground "p.fg_mid2"))))
 `(diff-added                            ((t (:background "p.green_bg" :foreground nil))))
 `(diff-changed                          ((t (:background nil :foreground nil))))
 `(diff-changed-unspecified              ((t (:background nil :foreground nil))))
 `(diff-context                          ((t (:background nil :foreground nil))))
 `(diff-error                            ((t (:background nil :foreground nil))))
 `(diff-file-header                      ((t (:background nil :foreground nil))))
 `(diff-function                         ((t (:background nil :foreground nil))))
 `(diff-header                           ((t (:background nil :foreground nil))))
 `(diff-hunk-header                      ((t (:background nil :foreground nil))))
 `(diff-index                            ((t (:background nil :foreground nil))))
 `(diff-indicator-added                  ((t (:background "p.green_bg" :foreground nil))))
 `(diff-indicator-changed                ((t (:background nil :foreground nil))))
 `(diff-indicator-removed                ((t (:background "p.red_bg" :foreground nil))))
 `(diff-nonexistent                      ((t (:background nil :foreground nil))))
 `(diff-refine-added                     ((t (:background nil :foreground nil))))
 `(diff-refine-changed                   ((t (:background nil :foreground nil))))
 `(diff-refine-removed                   ((t (:background nil :foreground nil))))
 `(diff-removed                          ((t (:background "p.red_bg" :foreground nil))))
 `(vertical-border                       ((t (:background "p.bg" :foreground "p.accent")))))

;;

(provide 'early-init)

;;; early-init.el ends here
]]

H.init_template = [[
;;; init.el --- -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(defvar elpaca-installer-version 0.10)
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode))
(elpaca (leaf))
(elpaca (leaf-keywords)
  (leaf-keywords-init))
(elpaca (leaf-tree))
(elpaca (leaf-convert))

;;

(elpaca-wait)

;;

(leaf *evil
      :config
      (leaf evil
            :elpaca t
            :preface
            (setq evil-want-integration t)
            (setq evil-want-keybinding nil)
            :config
            (evil-mode 1))
      (leaf evil-collection
            :elpaca t
            :init (evil-collection-init)))

;;

(leaf *magit
      :config
      (leaf magit
            :doc "It's Magit! A Git porcelain inside Emacs."
            :url "https://github.com/magit/magit"
            :elpaca t
            :setq ((magit-commit-diff-inhibit-same-window . t)
                   (magit-save-repository-buffers . 'dontask)))
      (leaf forge
            :doc "Work with Git forges from the comfort of Magit"
            :url "https://github.com/magit/forge"
            :elpaca t
            :setq (auth-sources '("~/.authinfo")))
      (leaf transient
            :doc "Transient commands"
            :url "https://github.com/magit/transient"
            :elpaca t))

;;

(elpaca-wait)

;;

(defun no-op ()
  "A no-op function for use with key bindings."
  (interactive))

(defun parse-command-line (args)
  "Handle specific command line arguments.
The reason why we don't use the Emacs hooks for processing user defined
arguments is that we want to process these arguments as soon as possible."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
          ("--magit"
           (evil-define-key 'normal magit-status-mode-map (kbd "q") 'no-op)
           (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
           (magit-status))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args))
  (delete-other-windows))

(setq command-line-args (parse-command-line command-line-args))

;;


;;
;; Neovim-Magit file interception for flatten.nvim
;;

;; Function to check if we're running inside Neovim
(defun inside-neovim-p ()
  "Return non-nil if running inside Neovim terminal."
  (getenv "NVIM_LISTEN_ADDRESS"))

;; Make the function interactive and add better error handling
(defun open-in-neovim (file)
  "Open FILE in the parent Neovim instance."
  (interactive "fOpen file in Neovim: ")
  (let ((file-path (expand-file-name file))
        (nvim-address (getenv "NVIM_LISTEN_ADDRESS")))
    (if (not nvim-address)
        (progn
          (message "Not running inside Neovim (no NVIM_LISTEN_ADDRESS)")
          nil)
      ;; Try different methods to communicate with Neovim
      (condition-case err
          (progn
            ;; Method 1: Use nvim directly (flatten.nvim will intercept)
            (call-process "nvim" nil nil nil nil file-path)
            (message "Opening %s in Neovim via flatten.nvim (nvim)" file-path)
            t)
        (error
         ;; Method 2: If nvim fails, try explicit RPC command
         (condition-case err2
             (progn
               (call-process "nvim" nil nil nil
                            "--server" nvim-address
                            "--remote-send"
                            (format "<C-\\><C-n>:edit %s<CR>" file-path))
               (message "Opening %s in Neovim via direct RPC" file-path)
               t)
           (error
            ;; Both methods failed, notify user
            (message "Failed to open in Neovim: %s" (error-message-string err2))
            nil)))))))

;; Advice around file-opening functions in Magit
(advice-add 'find-file :around
            (lambda (orig-fun &rest args)
              (if (and (inside-neovim-p)
                       (string-match-p "magit" (buffer-name)))
                  (or (open-in-neovim (car args))
                      (apply orig-fun args))
                (apply orig-fun args))))

;; Also advice `magit-find-file` specifically
(advice-add 'magit-find-file :around
            (lambda (orig-fun &rest args)
              (if (inside-neovim-p)
                  (let ((file (or (apply #'magit-file-at-point args) "")))
                    (if (and file (not (string= file "")))
                        (open-in-neovim file)
                      (apply orig-fun args)))
                (apply orig-fun args))))

;; And magit-display-file
(advice-add 'magit-display-file :around
            (lambda (orig-fun &rest args)
              (if (inside-neovim-p)
                  (or (open-in-neovim (car args))
                (apply orig-fun args))
              (apply orig-fun args))))

;; NEW: Intercept diff visiting functions (used in commit view)
(dolist (fn '(magit-diff-visit-file
              magit-diff-visit-file-other-window
              magit-diff-visit-file-worktree
              magit-diff-visit-worktree-file))
  (advice-add fn :around
              (lambda (orig-fun &rest args)
                (if (inside-neovim-p)
                  (let* ((section (magit-current-section))
                         (file (magit-file-at-point)))
                    (if file
                        (open-in-neovim file)
                      (apply orig-fun args)))
                  (apply orig-fun args)))))

;; NEW: Intercept file showing from logs and commits
(dolist (fn '(magit-show-commit
              magit-log-show-commit
              magit-show-file-revision))
  (advice-add fn :around
              (lambda (orig-fun &rest args)
                (if (inside-neovim-p)
                    (let ((file (magit-file-at-point)))
                      (if file
                          (open-in-neovim file)
                        (apply orig-fun args)))
                  (apply orig-fun args)))))

;; Define a fallback key binding for force-opening in Neovim
(when (inside-neovim-p)
  (dolist (map '(magit-file-section-map
                 magit-hunk-section-map
                 magit-commit-section-map
                 magit-log-section-map
                 magit-diff-section-map))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "C-c C-o") 'open-in-neovim))))

(provide 'init)

;;; init.el ends here
]]

--- Module setup
---
---@param config table|nil Module config table. See |Magit.config|.
---
---@usage `require('magit').setup({})` (replace `{}` with your `config` table)
Magit.setup = function (config)
  -- Export module
  _G.Magit = Magit

  -- Setup config
  config = H.setup_config (config)

  -- Generate Elisp files first
  H.ensure_elisp_files ()

  -- Update config with proper init directory before applying it
  -- This ensures the correct command is always used
  local elisp_dir = vim.fn.expand (config.elisp_dir)
  if not config.emacs_cmd:find ("--init%-directory") then
    config.emacs_cmd = config.emacs_cmd:gsub ("emacs%s+-nw", "emacs -nw --init-directory=" .. elisp_dir)
  end

  -- Now save the final config
  Magit.config = config

  -- Apply config with the corrected command
  H.apply_config (config)

  -- Create highlighting
  H.create_default_hl ()

  -- Define behavior
  H.create_autocommands ()
  H.create_user_commands ()

  -- Set up colorscheme synchronization
  H.setup_colorscheme_sync ()

  -- Set up file interception from Magit to Neovim
  H.setup_file_interception ()
end

--- Module config
---
--- Default values:
---@eval return MiniDoc.afterlines_to_code(MiniDoc.current.eval_section)
Magit.config = {
  -- Command to run Emacs with Magit
  -- Will be automatically updated with correct --init-directory path
  emacs_cmd = "emacs -nw --magit .",

  -- Directory for Elisp files (will be created if it doesn't exist)
  elisp_dir = vim.fn.stdpath ("data") .. "/magit",

  -- Terminal direction: 'float', 'tab', 'vertical', 'horizontal'
  direction = "tab",

  -- Terminal window size (for non-tab directions)
  -- Can be a number or a function returning a number
  size = function ()
    return math.floor (vim.o.columns * 0.8)
  end,

  -- Float window settings (when direction = 'float')
  float = {
    border = "rounded",
    width = function ()
      return math.floor (vim.o.columns * 0.8)
    end,
    height = function ()
      return math.floor (vim.o.lines * 0.8)
    end,
    -- Auto-center window (set row=nil and col=nil)
    row = nil,
    col = nil,
    title = " Magit ",
    title_pos = "center",
    zindex = 50,
    -- Window padding
    padding = { 1, 1, 1, 1 }, -- top, right, bottom, left
  },

  -- Automatically start in insert mode
  start_in_insert = true,

  -- Persist terminal mode between toggles
  persist_mode = true,

  -- Automatically change to the git root directory
  auto_chdir = true,

  -- Keymaps in terminal mode
  mappings = {
    toggle = "<leader>g",
    -- quit = "q",
    exit_terminal = "<C-\\><C-n>",
  },

  -- Hooks for customizing behavior
  hooks = {
    before_open = nil, -- function() end
    after_open = nil, -- function(term) end
    after_close = nil, -- function() end
  },

  -- Synchronize colorscheme between Neovim and Emacs
  sync_colorscheme = true,
}

-- Module functionality =======================================================

--- Toggle Magit terminal
---
---@param opts table|nil Options to override config (same structure as config)
Magit.toggle = function (opts)
  opts = vim.tbl_deep_extend ("force", Magit.config, opts or {})

  -- If terminal exists and is open, close it
  if H.term and H.term:is_open () then
    H.term:close ()
    if type (opts.hooks.after_close) == "function" then
      opts.hooks.after_close ()
    end
    return
  end

  -- Run before_open hook if defined
  if type (opts.hooks.before_open) == "function" then
    opts.hooks.before_open ()
  end

  -- Change to git root if auto_chdir is enabled
  if opts.auto_chdir then
    H.change_to_git_root ()
  end

  -- Create terminal if it doesn't exist
  if not H.term then
    H.term = H.create_terminal (opts)
  end

  -- Open the terminal
  H.term:open ()

  -- Run after_open hook if defined
  if type (opts.hooks.after_open) == "function" then
    opts.hooks.after_open (H.term)
  end
end

--- Close Magit terminal if open
Magit.close = function ()
  if H.term and H.term:is_open () then
    H.term:close ()
    if type (Magit.config.hooks.after_close) == "function" then
      Magit.config.hooks.after_close ()
    end
  end
end

-- Helper functionality =======================================================

-- Terminal instance
H.term = nil

-- Terminal class
H.Terminal = {}
H.Terminal.__index = H.Terminal

--- Create a new terminal instance
---@param opts table Terminal options
---@return table Terminal instance
H.create_terminal = function (opts)
  local term = setmetatable ({}, H.Terminal)
  term.opts = opts
  term.bufnr = nil
  term.winid = nil
  term.job_id = nil
  term.mode = opts.start_in_insert and "i" or "n"
  return term
end

--- Open the terminal
function H.Terminal:open ()
  local opts = self.opts

  -- Create buffer if it doesn't exist
  if not self.bufnr or not vim.api.nvim_buf_is_valid (self.bufnr) then
    self.bufnr = vim.api.nvim_create_buf (false, true)
    vim.api.nvim_buf_set_option (self.bufnr, "bufhidden", "hide")
    vim.api.nvim_buf_set_var (self.bufnr, "magit", true)

    -- Set a meaningful name for the buffer
    vim.api.nvim_buf_set_name (self.bufnr, "Magit")
  end

  -- Create window based on direction
  if opts.direction == "tab" then
    vim.cmd.tabnew ()
    self.winid = vim.api.nvim_get_current_win ()
    vim.api.nvim_win_set_buf (self.winid, self.bufnr)
  elseif opts.direction == "float" then
    local float_opts = H.get_float_options (opts.float)
    self.winid = vim.api.nvim_open_win (self.bufnr, true, float_opts)
  elseif opts.direction == "vertical" then
    local size = H.get_size (opts.size)
    vim.cmd (size .. "vsplit")
    self.winid = vim.api.nvim_get_current_win ()
    vim.api.nvim_win_set_buf (self.winid, self.bufnr)
  elseif opts.direction == "horizontal" then
    local size = H.get_size (opts.size)
    vim.cmd (size .. "split")
    self.winid = vim.api.nvim_get_current_win ()
    vim.api.nvim_win_set_buf (self.winid, self.bufnr)
  end

  -- Start terminal if not already started
  if not self.job_id then
    -- FIXME: termopen is deprecated
    self.job_id = vim.fn.termopen (self.opts.emacs_cmd, {
      on_exit = function (_, _, _)
        if vim.api.nvim_buf_is_valid (self.bufnr) then
          if self:is_open () then
            self:close ()
          end
          -- Clean up to allow reopening
          self.job_id = nil
        end
      end,
    })

    -- FIXME: Setup keymaps for quitting
    -- local quit_key = self.opts.mappings.quit
    -- if quit_key and quit_key ~= "" then
    --   vim.api.nvim_buf_set_keymap(self.bufnr, "t", quit_key,
    --     "<C-\\><C-n>:lua Magit.close()<CR>", {noremap = true, silent = true})
    -- end
  end

  -- Use last mode if persisting, otherwise use configured start mode
  if self.opts.persist_mode then
    if self.mode == "i" then
      vim.cmd.startinsert ()
    else
      vim.cmd.stopinsert ()
    end
  elseif self.opts.start_in_insert then
    vim.cmd.startinsert ()
  else
    vim.cmd.stopinsert ()
  end

  -- Set up autocommand to track terminal mode
  if self.opts.persist_mode and not H.mode_autocmd_set then
    vim.api.nvim_create_autocmd ({ "TermEnter", "TermLeave" }, {
      buffer = self.bufnr,
      callback = function (args)
        if args.event == "TermEnter" then
          self.mode = "i"
        else
          self.mode = "n"
        end
      end,
    })
    H.mode_autocmd_set = true
  end
end

--- Close the terminal
function H.Terminal:close ()
  if self.winid and vim.api.nvim_win_is_valid (self.winid) then
    if self.opts.persist_mode then
      self.mode = vim.api.nvim_get_mode ().mode
    end

    if self.opts.direction == "tab" then
      vim.cmd.tabclose ()
    else
      vim.api.nvim_win_close (self.winid, true)
    end
    self.winid = nil
  end
end

--- Check if terminal is open
---@return boolean
function H.Terminal:is_open ()
  return self.winid ~= nil and vim.api.nvim_win_is_valid (self.winid)
end

--- Set up and validate config
---@param config table|nil User configuration table
---@return table Validated and merged configuration
H.setup_config = function (config)
  config = vim.tbl_deep_extend ("force", vim.deepcopy (Magit.config), config or {})

  local valid_directions = { "float", "tab", "vertical", "horizontal" }
  if not vim.tbl_contains (valid_directions, config.direction) then
    vim.notify ("Magit: Invalid direction '" .. config.direction .. "'. Using 'tab' instead.", vim.log.levels.WARN)
    config.direction = "tab"
  end

  config.elisp_dir = vim.fn.expand (config.elisp_dir)

  return config
end

--- Apply configuration settings
---@param config table Configuration table
---@return nil
H.apply_config = function (config)
  if config.mappings.toggle then
    vim.keymap.set ("n", config.mappings.toggle, Magit.toggle, { desc = "Magit" })
  end
end

--- Create default highlight groups
---@return nil
H.create_default_hl = function ()
  local hi = vim.api.nvim_set_hl
  hi (0, "MagitBorder", { link = "FloatBorder", default = true })
  hi (0, "MagitFloat", { link = "NormalFloat", default = true })
end

--- Create autocommands
---@return nil
H.create_autocommands = function ()
  local augroup = vim.api.nvim_create_augroup ("Magit", { clear = true })

  vim.api.nvim_create_autocmd ("TermEnter", {
    group = augroup,
    pattern = "term://*",
    callback = function (args)
      local bufnr = args.buf
      local ok, is_magit = pcall (vim.api.nvim_buf_get_var, bufnr, "magit")
      if ok and is_magit then
        vim.defer_fn (function ()
          if vim.api.nvim_buf_is_valid (bufnr) then
            vim.cmd.startinsert ()
          end
        end, 10)
      end
    end,
  })

  vim.api.nvim_create_autocmd ("BufEnter", {
    group = augroup,
    pattern = "term://*",
    callback = function (args)
      local bufnr = args.buf
      local ok, is_magit = pcall (vim.api.nvim_buf_get_var, bufnr, "magit")
      if ok and is_magit then
        vim.defer_fn (function ()
          if vim.api.nvim_buf_is_valid (bufnr) then
            vim.cmd.startinsert ()
          end
        end, 10)
      end
    end,
  })

  -- Handle command line interactions
  vim.api.nvim_create_autocmd ({ "ModeChanged" }, {
    group = augroup,
    pattern = { "*:c", "c:*" },
    callback = function ()
      if not H.term or not H.term.bufnr then
        return
      end

      -- When leaving cmdline mode, check if we should go back to insert mode in magit
      vim.defer_fn (function ()
        if not H.term.bufnr or not vim.api.nvim_buf_is_valid (H.term.bufnr) then
          return
        end

        local current_buf = vim.api.nvim_get_current_buf ()
        if current_buf == H.term.bufnr then
          vim.cmd.startinsert ()
        end
      end, 10)
    end,
  })

  -- Handle mouse clicks and cursor movements
  vim.api.nvim_create_autocmd ({ "BufEnter", "FocusGained" }, {
    group = augroup,
    callback = function ()
      if not H.term or not H.term.bufnr then
        return
      end

      -- Set up cursor movement handling for the current buffer
      local current_buf = vim.api.nvim_get_current_buf ()
      if current_buf == H.term.bufnr then
        -- Ensure that magit buffer has mouse support via cursor movements
        vim.api.nvim_create_autocmd ({ "CursorMoved", "CursorMovedI" }, {
          group = augroup,
          buffer = current_buf,
          callback = function ()
            if vim.fn.mode () ~= "i" then
              vim.defer_fn (function ()
                if vim.api.nvim_get_current_buf () == current_buf then
                  vim.cmd.startinsert ()
                end
              end, 50)
            end
          end,
          once = false,
        })
      end
    end,
  })

  -- Special handling for mode changes to normal mode
  vim.api.nvim_create_autocmd ("ModeChanged", {
    group = augroup,
    pattern = "*:n",
    callback = function ()
      if not H.term or not H.term.bufnr then return end
      local current_buf = vim.api.nvim_get_current_buf ()
      if current_buf == H.term.bufnr then
        vim.defer_fn (function ()
          if vim.api.nvim_get_current_buf () == current_buf then
            vim.cmd.startinsert ()
          end
        end, 10)
      end
    end,
  })
end

--- Create user commands
---@return nil
H.create_user_commands = function ()
  vim.api.nvim_create_user_command ("Magit", function (opts)
    Magit.toggle ()
  end, {
    desc = "Toggle Magit",
  })
end

--- Set terminal keymaps
---@param bufnr number Buffer number
---@param opts table Terminal options
---@return nil
H.set_terminal_keymaps = function (bufnr, opts)
  local map_opts = { buffer = bufnr, noremap = true, silent = true }

  -- Exit terminal mode
  vim.keymap.set ("t", opts.mappings.exit_terminal, "<C-\\><C-n>", map_opts)

  -- Allow colon to trigger command mode
  vim.keymap.set ("t", ":", [[<C-\><C-n>:]], map_opts)

  -- Add C-o support
  vim.keymap.set ("t", "<C-o>", [[<C-\><C-n><C-o>]], map_opts)

  -- FIXME: Magit also use q internally
  --
  -- Quit magit with 'q' in normal mode
  -- vim.keymap.set("n", opts.mappings.quit, function()
  --   if H.term then
  --     H.term:close()
  --     if type(opts.hooks.after_close) == "function" then
  --       opts.hooks.after_close()
  --     end
  --   end
  -- end, map_opts)
end

--- Get float window options
---@param float_config table Float configuration
---@return table Float options
H.get_float_options = function (float_config)
  local width = H.get_size (float_config.width)
  local height = H.get_size (float_config.height)

  -- Calculate centered position if not specified
  local col = float_config.col
  local row = float_config.row

  if col == nil then
    col = math.floor((vim.o.columns - width) / 2)
  end

  if row == nil then
    row = math.floor((vim.o.lines - height) / 2)
  end

  -- Extract other options with defaults
  local title = float_config.title or " Magit "
  local title_pos = float_config.title_pos or "center"
  local zindex = float_config.zindex or 50

  return {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = float_config.border,
    title = title,
    title_pos = title_pos,
    zindex = zindex,
  }
end

--- Get size value
---@param size number|function Size value or function returning size
---@return number Size value
H.get_size = function (size)
  if type (size) == "function" then
    return size ()
  end
  return size
end

-- Colorscheme synchronization ================================================

--- Set up colorscheme synchronization
---@return nil
H.setup_colorscheme_sync = function ()
  if not Magit.config.sync_colorscheme then
    return
  end

  -- Schedule the initial color caching to avoid fast event context
  vim.schedule (function ()
    H.cache_colors ()
  end)

  vim.api.nvim_create_autocmd ("ColorScheme", {
    group = vim.api.nvim_create_augroup ("MagitColorSync", { clear = true }),
    callback = function ()
      -- Schedule the color caching to avoid fast event context
      vim.schedule (function ()
        H.cache_colors ()
        H.on_colorscheme_change ()
      end)
    end,
  })

  -- Schedule the initial colorscheme application
  vim.schedule (function ()
    H.on_colorscheme_change ()
  end)
end

-- Cached colors to avoid API calls in fast events
H.color_cache = {}

--- Cache colors for later use
H.cache_colors = function ()
  local ok, mini_hues = pcall (require, "mini.hues")
  if not ok then
    -- Set default colors if mini.hues is not available
    H.color_cache = {
      bg = vim.o.background == "dark" and "#000000" or "#ffffff",
      fg = vim.o.background == "dark" and "#ffffff" or "#000000",
      -- NOTE: Add more default colors as needed
    }
    return
  end

  -- Get highlight colors safely
  local normal_hl = vim.api.nvim_get_hl (0, { name = "Normal" })

  -- Extract and validate background color
  local background
  if normal_hl and normal_hl.bg then
    background = normal_hl.bg
    -- Convert number to hex if needed
    if type (background) == "number" then
      background = string.format ("#%06x", background)
    end
  else
    background = vim.o.background == "dark" and "#000000" or "#ffffff"
  end

  -- Extract and validate foreground color
  local foreground
  if normal_hl and normal_hl.fg then
    foreground = normal_hl.fg
    -- Convert number to hex if needed
    if type (foreground) == "number" then
      foreground = string.format ("#%06x", foreground)
    end
  else
    foreground = vim.o.background == "dark" and "#ffffff" or "#000000"
  end

  -- Ensure colors are valid hex colors
  if type (background) ~= "string" or not background:match ("^#%x%x%x%x%x%x$") then
    background = vim.o.background == "dark" and "#000000" or "#ffffff"
  end
  if type (foreground) ~= "string" or not foreground:match ("^#%x%x%x%x%x%x$") then
    foreground = vim.o.background == "dark" and "#ffffff" or "#000000"
  end

  -- Try to create palette, falling back to defaults if it fails
  local palette_opts = { background = background, foreground = foreground, }
  local success, palette = pcall (mini_hues.make_palette, palette_opts)
  if not success then
    H.color_cache = {
      bg = background,
      fg = foreground,
      -- Add fallback colors for other common values
      red = "#ff5555",
      green = "#50fa7b",
      blue = "#8be9fd",
      yellow = "#f1fa8c",
      -- NOTE: Add more default colors as needed
    }
    return
  end

  H.color_cache = palette
end

--- Get accent color from the cached palette
---@param accent string The accent key for the color palette
---@return string The corresponding color value
H.get_accent_color = function (accent)
  -- Use cached color if available
  if H.color_cache[accent] then
    return H.color_cache[accent]
  end

  -- Fallback values if color not in cache
  if accent:match ("_bg$") then
    return H.color_cache.bg or (vim.o.background == "dark" and "#000000" or "#ffffff")
  else
    return H.color_cache.fg or (vim.o.background == "dark" and "#ffffff" or "#000000")
  end
end

--- Extract shared color replacement logic
---@param content string Content with color placeholders
---@return string Content with actual colors
H.replace_color_placeholders = function (content)
  -- Replace placeholder colors with actual colors
  content = content:gsub ("p%.bg_mid2", H.get_accent_color ("bg_mid2"))
  content = content:gsub ("p%.bg", H.get_accent_color ("bg"))
  content = content:gsub ("p%.fg_mid2", H.get_accent_color ("fg_mid2"))
  content = content:gsub ("p%.fg", H.get_accent_color ("fg"))
  content = content:gsub ("p%.red_bg", H.get_accent_color ("red_bg"))
  content = content:gsub ("p%.orange_bg", H.get_accent_color ("orange_bg"))
  content = content:gsub ("p%.yellow_bg", H.get_accent_color ("yellow_bg"))
  content = content:gsub ("p%.green_bg", H.get_accent_color ("green_bg"))
  content = content:gsub ("p%.cyan_bg", H.get_accent_color ("cyan_bg"))
  content = content:gsub ("p%.azure_bg", H.get_accent_color ("azure_bg"))
  content = content:gsub ("p%.blue_bg", H.get_accent_color ("blue_bg"))
  content = content:gsub ("p%.purple_bg", H.get_accent_color ("purple_bg"))
  content = content:gsub ("p%.red", H.get_accent_color ("red"))
  content = content:gsub ("p%.orange", H.get_accent_color ("orange"))
  content = content:gsub ("p%.yellow", H.get_accent_color ("yellow"))
  content = content:gsub ("p%.green", H.get_accent_color ("green"))
  content = content:gsub ("p%.cyan", H.get_accent_color ("cyan"))
  content = content:gsub ("p%.azure", H.get_accent_color ("azure"))
  content = content:gsub ("p%.blue", H.get_accent_color ("blue"))
  content = content:gsub ("p%.purple", H.get_accent_color ("purple"))
  content = content:gsub ("p%.accent", H.get_accent_color ("accent"))

  return content
end

--- Replace placeholder colors in a file with actual accent colors (async)
---@param file_path string The path of the file to be modified
H.replace_with_accent = function (file_path)
  local uv = vim.uv
  uv.fs_open (file_path, "r", 438, function (err, fd)
    if err then
      vim.schedule (function ()
        vim.notify ("Magit: Unable to open file " .. file_path, vim.log.levels.ERROR)
      end)
      return
    end
    uv.fs_fstat (fd, function (_, stat)
      assert (stat ~= nil)
      uv.fs_read (fd, stat.size, 0, function (_, data)
        uv.fs_close (fd, function ()
          assert (data ~= nil)
          local updated_data = H.replace_color_placeholders (data)
          uv.fs_open (file_path, "w", 438, function (_, fd_w)
            uv.fs_write (fd_w, updated_data, 0, function ()
              uv.fs_close (fd_w)
            end)
          end)
        end)
      end)
    end)
  end)
end

--- Apply colorscheme to an Elisp file (sync)
---@param file_path string Path to the Elisp file
---@return nil
H.apply_colorscheme = function (file_path)
  H.cache_colors ()

  local file = io.open (file_path, "r")
  if not file then
    vim.notify ("Failed to open file for colorscheme update: " .. file_path, vim.log.levels.ERROR)
    return
  end
  local content = file:read ("*all")
  file:close ()

  local updated_content = H.replace_color_placeholders (content)
  local out_file = io.open (file_path, "w")
  if out_file then
    out_file:write (updated_content)
    out_file:close ()
  end
end

--- Handle colorscheme change event
---@return nil
H.on_colorscheme_change = function ()
  local config = Magit.config
  local elisp_dir = vim.fn.expand(config.elisp_dir)
  local dest_file = elisp_dir .. "/early-init.el"

  -- Use the template from our own code instead of an external file
  H.write_file(dest_file, H.early_init_template)

  -- Apply colorscheme immediately
  H.replace_with_accent(dest_file)
end

--- Change to git root directory
---@return nil
H.change_to_git_root = function ()
  local ok, result = pcall (function () return vim.fn.systemlist ("git rev-parse --show-toplevel")[1] end)
  if ok and result and result ~= "" then
    vim.cmd ("lcd " .. result)
  end
end

--- Setup file interception from Magit to Neovim
H.setup_file_interception = function ()
  -- Modify Emacs command to include NVIM_LISTEN_ADDRESS.
  if pcall (require, "flatten") then
    local original_cmd = Magit.config.emacs_cmd
    if not original_cmd:find ("NVIM_LISTEN_ADDRESS") then
      Magit.config.emacs_cmd = "NVIM_LISTEN_ADDRESS=" .. vim.v.servername .. " " .. original_cmd
    end
    return
  end
end

--- Ensure Elisp files exist and are up to date
---@return nil
H.ensure_elisp_files = function ()
  local elisp_dir = vim.fn.expand(Magit.config.elisp_dir)
  local init_file = elisp_dir .. "/init.el"
  local early_init_file = elisp_dir .. "/early-init.el"

  if vim.fn.isdirectory(elisp_dir) ~= 1 then
    vim.fn.mkdir(elisp_dir, "p")
  end

  H.write_file(init_file, H.init_template)
  H.write_file(early_init_file, H.early_init_template)

  -- Apply colorscheme immediately if sync is enabled
  if Magit.config.sync_colorscheme then
    H.apply_colorscheme(early_init_file)
  end
end

--- Write content to a file
---@param file_path string Path to the file
---@param content string Content to write
---@return nil
H.write_file = function (file_path, content)
  local file = io.open (file_path, "w")
  if file then
    file:write (content)
    file:close ()
  else
    vim.notify ("Failed to write file: " .. file_path, vim.log.levels.ERROR)
  end
end

return Magit
