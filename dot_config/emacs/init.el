;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;


;;; Code:

(set-frame-parameter nil 'undecorated t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
(elpaca `(,@elpaca-order))
(when (member system-type '(windows-nt ms-dos))
  (elpaca-no-symlink-mode))

;;

(elpaca (leaf))
(elpaca (leaf-keywords)
  (leaf-keywords-init))

;;

(elpaca-wait)

;;

(leaf ef-themes
  :elpaca t)

(leaf modus-themes
  :elpaca t)

(leaf doric-themes
  :elpaca (doric-themes :host github :repo "protesilaos/doric-themes"))

(leaf remember-last-theme
  :elpaca t
  :init (remember-last-theme-enable))

;;

(elpaca-wait)

;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'dotemacs-context-menu)

;;

(leaf abbrev
  :doc "Toggle Abbrev mode in the current buffer"
  :tag "builtin" "text")

(leaf allout
  :doc "Toggle Allout outline mode"
  :tag "builtin" "outline")

(leaf allout-widgets
  :doc "Toggle Allout Widgets mode"
  :tag "builtin" "outline")

(leaf artist
  :doc "Toggle Artist mode"
  :tag "builtin" "graphics")

(leaf auto-composition
  :doc "Toggle Auto Composition mode"
  :tag "builtin" "composition")

(leaf auto-compression
  :doc "Toggle Auto Compression mode"
  :tag "builtin" "compression")

(leaf auto-encryption
  :doc "Toggle automatic file encryption/decryption (Auto Encryption mode)"
  :tag "builtin" "encryption")

(leaf auto-fill
  :doc "Toggle automatic line breaking (Auto Fill mode)"
  :tag "builtin" "text")

(leaf auto-image-file
  :doc "Toggle visiting of image files as images (Auto Image File mode)"
  :tag "builtin" "image")

(leaf auto-insert
  :doc "Toggle Auto-insert mode, a global minor mode"
  :tag "builtin" "convenience")

(leaf auto-revert
  :doc "Toggle reverting buffer when the file changes (Auto-Revert Mode)"
  :tag "builtin" "files"
  :custom (;; Disable periodic polling. We can trust file notifications to trigger
           ;; updates in the common case.
           ;;
           (auto-revert-avoid-polling . 1))
  :global-minor-mode t)

(leaf auto-revert-tail
  :doc "Toggle reverting tail of buffer when the file grows"
  :tag "builtin" "files")

(leaf auto-save
  :doc "Toggle auto-saving in the current buffer (Auto Save mode)"
  :tag "builtin" "files")

(leaf auto-save-visited
  :doc "Toggle automatic saving of file-visiting buffers to their files"
  :tag "builtin" "files")

(leaf blink-cursor
  :doc "Toggle cursor blinking (Blink Cursor mode)"
  :tag "builtin" "cursor")

(leaf buffer-face
  :doc "Minor mode for a buffer-specific default face"
  :tag "builtin" "faces")

(leaf bug-reference
  :doc "Toggle hyperlinking bug references in the buffer (Bug Reference mode)"
  :tag "builtin" "convenience")

(leaf bug-reference-prog
  :doc "Like 'bug-reference', but only buttonize in comments and strings"
  :tag "builtin" "convenience")

(leaf button
  :doc "A minor mode for navigating to buttons with the TAB key"
  :tag "builtin" "navigation")

(leaf checkdoc-minor
  :doc "Toggle automatic docstring checking (Checkdoc minor mode)"
  :tag "builtin" "lisp")

(leaf cl-font-lock-built-in
  :doc "Highlight built-in functions, variables, and types in 'lisp'"
  :tag "builtin" "lisp")

(leaf cl-old-struct-compat
  :doc "Enable backward compatibility with old-style structs"
  :tag "builtin" "lisp")

(leaf column-number
  :doc "Toggle column number display in the mode line (Column Number mode)"
  :tag "builtin" "mode-line")

(leaf comint-fontify-input
  :doc "Enable input fontification in the current comint buffer"
  :tag "builtin" "comint")

(leaf compilation-minor
  :doc "Toggle Compilation minor mode"
  :tag "builtin" "compilation")

(leaf compilation-shell-minor
  :doc "Toggle Compilation Shell minor mode"
  :tag "builtin" "compilation"
  :hook ((compilation-mode . compilation-shell-minor-mode)))

(leaf completion-in-region
  :doc "Transient minor mode used during 'completion-in-region'"
  :tag "builtin" "completion")

(leaf completion-preview
  :doc "Show in-buffer completion suggestions in a preview as you type"
  :tag "builtin" "completion")

(leaf context-menu
  :doc "Toggle Context Menu mode"
  :tag "builtin" "menu"
  :global-minor-mode t)

(leaf cperl-extra-paired-delimiters
  :doc "Toggle treatment of extra paired delimiters in Perl"
  :tag "builtin" "perl")

(leaf cua
  :doc "Toggle Common User Access style editing (CUA mode)"
  :tag "builtin" "editing"
  :global-minor-mode t)

(leaf cua-rectangle-mark
  :doc "Toggle the region as rectangular"
  :tag "builtin" "editing")

(leaf cursor-face-highlight
  :doc "When enabled, highlight text that has 'cursor-face' property near point"
  :tag "builtin" "cursor")

(leaf cursor-intangible
  :doc "Keep cursor outside of any 'cursor-intangible' text property"
  :tag "builtin" "cursor")

(leaf cursor-sensor
  :doc "Handle the 'cursor-sensor-functions' text property"
  :tag "builtin" "cursor")

(leaf cvs-minor
  :doc "This mode is used for buffers related to a main *cvs* buffer"
  :tag "builtin" "vc")

(leaf cwarn
  :doc "Minor mode that highlights suspicious C and C++ constructions"
  :tag "builtin" "c")

(leaf delete-selection
  :doc "Toggle Delete Selection mode"
  :tag "builtin" "editing"
  :global-minor-mode t)

(leaf desktop-save
  :doc "Toggle desktop saving (Desktop Save mode)"
  :tag "builtin" "session")

(leaf dictionary-tooltip
  :doc "Display tooltips for the current word"
  :tag "builtin" "dictionary")

(leaf diff-auto-refine
  :doc "Toggle automatic diff hunk finer highlighting (Diff Auto Refine mode)"
  :tag "builtin" "diff")

(leaf diff-minor
  :doc "Toggle Diff minor mode"
  :tag "builtin" "diff")

(leaf dired-click-to-select
  :doc "Toggle click-to-select inside this Dired buffer"
  :tag "builtin" "dired")

(leaf dired-hide-details
  :doc "Toggle visibility of detailed information in current Dired buffer"
  :tag "builtin" "dired")

(leaf dirtrack-debug
  :doc "Toggle Dirtrack debugging"
  :tag "builtin" "shell")

(leaf dirtrack
  :doc "Toggle directory tracking in shell buffers (Dirtrack mode)"
  :tag "builtin" "shell")

(leaf display-battery
  :doc "Toggle battery status display in mode line (Display Battery mode)"
  :tag "builtin" "mode-line")

(leaf display-fill-column-indicator
  :doc "Toggle display of 'fill-column' indicator"
  :tag "builtin" "display")

(leaf display-line-numbers
  :doc "Toggle display of line numbers in the buffer"
  :tag "builtin" "display")

(leaf display-time
  :doc "Toggle display of time, load level, and mail flag in mode lines"
  :tag "builtin" "mode-line")

(leaf doc-view-minor
  :doc "Toggle displaying buffer via Doc View (Doc View minor mode)"
  :tag "builtin" "doc-view")

(leaf doc-view-presentation
  :doc "Minor mode used while in presentation mode"
  :tag "builtin" "doc-view")

(leaf double
  :doc "Toggle special insertion on double keypresses (Double mode)"
  :tag "builtin" "input")

(leaf dynamic-completion
  :doc "Toggle dynamic word-completion on or off"
  :tag "builtin" "completion")

(leaf ede-dired-minor
  :doc "A minor mode that should only be activated in DIRED buffers"
  :tag "builtin" "ede")

(leaf ede-minor
  :doc "Toggle EDE (Emacs Development Environment) minor mode"
  :tag "builtin" "ede")

(leaf editorconfig
  :doc "Toggle EditorConfig feature"
  :tag "external" "editorconfig"
  :global-minor-mode t
  :custom ((editorconfig-get-properties-function . #'editorconfig-get-properties)))

(leaf eldoc
  :doc "Toggle echo area display of Lisp objects at point (ElDoc mode)"
  :tag "builtin" "documentation")

(leaf electric-indent
  :doc "Toggle on-the-fly reindentation of text lines (Electric Indent mode)"
  :tag "builtin" "editing")

(leaf electric-layout
  :doc "Automatically insert newlines around some chars"
  :tag "builtin" "editing")

(leaf electric-pair
  :doc "Toggle automatic parens pairing (Electric Pair mode)"
  :tag "builtin" "editing")

(leaf electric-quote
  :doc "Toggle on-the-fly requoting (Electric Quote mode)"
  :tag "builtin" "editing")

(leaf elide-head
  :doc "Toggle eliding (hiding) header material in the current buffer"
  :tag "builtin" "convenience")

(leaf emacs-lock
  :doc "Toggle Emacs Lock mode in the current buffer"
  :tag "builtin" "convenience")

(leaf enriched
  :doc "Minor mode for editing text/enriched files"
  :tag "builtin" "text")

(leaf epa-global-mail
  :doc "Minor mode to hook EasyPG into Mail mode"
  :tag "builtin" "encryption")

(leaf epa-mail
  :doc "A minor for composing encrypted/clearsigned mails"
  :tag "builtin" "encryption")

(leaf eshell-arg
  :doc "Minor mode for the arg eshell module"
  :tag "builtin" "eshell")

(leaf eshell-proc
  :doc "Minor mode for the proc eshell module"
  :tag "builtin" "eshell")

(leaf eshell-var
  :doc "Minor mode for the esh-var module"
  :tag "builtin" "eshell")

(leaf etags-regen
  :doc "Minor mode to automatically generate and update tags tables"
  :tag "builtin" "tags")

(leaf fido
  :doc "An enhanced 'icomplete' that emulates 'ido'"
  :tag "builtin" "completion")

(leaf fido-vertical
  :doc "Toggle vertical candidate display in 'fido'"
  :tag "builtin" "completion")

(leaf file-name-shadow
  :doc "Toggle file-name shadowing in minibuffers (File-Name Shadow mode)"
  :tag "builtin" "files")

(leaf flymake
  :doc "Toggle Flymake mode on or off"
  :tag "builtin" "syntax-checking")

(leaf flyspell
  :doc "Toggle on-the-fly spell checking (Flyspell mode)"
  :tag "builtin" "spell-checking")

(leaf follow
  :doc "Toggle Follow mode"
  :tag "builtin" "windows")

(leaf font-lock
  :doc "Toggle syntax highlighting in this buffer (Font Lock mode)"
  :tag "builtin" "syntax-highlighting")

(leaf footnote
  :doc "Toggle Footnote mode"
  :tag "builtin" "text")

(leaf glasses
  :doc "Minor mode for making identifiers likeThis readable"
  :tag "builtin" "programming")

(leaf global-auto-revert
  :doc "Toggle Global Auto-Revert Mode"
  :tag "builtin" "files"
  :global-minor-mode t)

(leaf global-completion-preview
  :doc "Toggle Completion-Preview mode in all buffers"
  :tag "builtin" "completion")

(leaf global-cwarn
  :doc "Toggle Cwarn mode in all buffers"
  :tag "builtin" "c")

(leaf global-display-fill-column-indicator
  :doc "Toggle Display-Fill-Column-Indicator mode in all buffers"
  :tag "builtin" "display")

(leaf global-display-line-numbers
  :doc "Toggle Display-Line-Numbers mode in all buffers"
  :tag "builtin" "display")

(leaf global-ede
  :doc "Toggle global EDE (Emacs Development Environment) mode"
  :tag "builtin" "ede")

(leaf global-eldoc
  :doc "Toggle Eldoc mode in all buffers"
  :tag "builtin" "documentation")

(leaf global-font-lock
  :doc "Toggle Font-Lock mode in all buffers"
  :tag "builtin" "syntax-highlighting")

(leaf global-goto-address
  :doc "Toggle Goto-Address mode in all buffers"
  :tag "builtin" "convenience")

(leaf global-hi-lock
  :doc "Toggle Hi-Lock mode in all buffers"
  :tag "builtin" "highlighting")

(leaf global-highlight-changes
  :doc "Toggle Highlight-Changes mode in all buffers"
  :tag "builtin" "highlighting")

(leaf global-hl-line
  :doc "Toggle line highlighting in all buffers (Global Hl-Line mode)"
  :tag "builtin" "highlighting")

(leaf global-prettify-symbols
  :doc "Toggle Prettify-Symbols mode in all buffers"
  :tag "builtin" "display")

(leaf global-reveal
  :doc "Toggle Reveal mode in all buffers (Global Reveal mode)"
  :tag "builtin" "display")

(leaf global-semantic-highlight-edits
  :doc "Toggle global use of option 'semantic-highlight-edits'"
  :tag "builtin" "semantic")

(leaf global-semantic-highlight-func
  :doc "Toggle global use of option 'semantic-highlight-func'"
  :tag "builtin" "semantic")

(leaf global-semantic-show-parser-state
  :doc "Toggle global use of option 'semantic-show-parser-state'"
  :tag "builtin" "semantic")

(leaf global-semantic-show-unmatched-syntax
  :doc "Toggle global use of option 'semantic-show-unmatched-syntax'"
  :tag "builtin" "semantic")

(leaf global-semantic-stickyfunc
  :doc "Toggle global use of option 'semantic-stickyfunc'"
  :tag "builtin" "semantic")

(leaf global-semanticdb-minor
  :doc "Toggle Semantic DB mode"
  :tag "builtin" "semantic")

(leaf global-so-long
  :doc "Toggle automated performance mitigations for files with long lines"
  :tag "builtin" "performance")

(leaf global-subword
  :doc "Toggle Subword mode in all buffers"
  :tag "builtin" "editing")

(leaf global-superword
  :doc "Toggle Superword mode in all buffers"
  :tag "builtin" "editing")

(leaf global-tab-line
  :doc "Toggle Tab-Line mode in all buffers"
  :tag "builtin" "tabs")

(leaf global-visual-line
  :doc "Toggle Visual-Line mode in all buffers"
  :tag "builtin" "text")

(leaf global-visual-wrap-prefix
  :doc "Toggle Visual-Wrap-Prefix mode in all buffers"
  :tag "builtin" "text")

(leaf global-whitespace
  :doc "Toggle Whitespace mode in all buffers"
  :tag "builtin" "whitespace")

(leaf global-whitespace-newline
  :doc "Toggle global newline visualization (Global Whitespace Newline mode)"
  :tag "builtin" "whitespace")

(leaf global-window-tool-bar
  :doc "Toggle Window-Tool-Bar mode in all buffers"
  :tag "builtin" "toolbar")

(leaf global-word-wrap-whitespace
  :doc "Toggle Word-Wrap-Whitespace mode in all buffers"
  :tag "builtin" "text")

(leaf glyphless-display
  :doc "Minor mode for displaying glyphless characters in the current buffer"
  :tag "builtin" "display")

(leaf gnus-binary
  :doc "Minor mode for providing a binary group interface in Gnus summary buffers"
  :tag "builtin" "gnus")

(leaf gnus-dead-summary
  :doc "Minor mode for Gnus summary buffers"
  :tag "builtin" "gnus")

(leaf gnus-dired
  :doc "Minor mode for intersections of gnus and Dired"
  :tag "builtin" "gnus")

(leaf gnus-draft
  :doc "Minor mode for providing a draft summary buffers"
  :tag "builtin" "gnus")

(leaf gnus-mailing-list
  :doc "Minor mode for providing mailing-list commands"
  :tag "builtin" "gnus")

(leaf gnus-message-citation
  :doc "Minor mode providing more font-lock support for nested citations"
  :tag "builtin" "gnus")

(leaf gnus-pick
  :doc "Minor mode for providing a pick-and-read interface in Gnus summary buffers"
  :tag "builtin" "gnus")

(leaf gnus-topic
  :doc "Minor mode for topicsifying Gnus group buffers"
  :tag "builtin" "gnus")

(leaf gnus-undo
  :doc "Minor mode for providing 'undo' in Gnus buffers"
  :tag "builtin" "gnus")

(leaf goto-address
  :doc "Minor mode to buttonize URLs and e-mail addresses in the current buffer"
  :tag "builtin" "convenience")

(leaf goto-address-prog
  :doc "Like 'goto-address', but only for comments and strings"
  :tag "builtin" "convenience")

(leaf gpm-mouse
  :doc "Toggle mouse support in GNU/Linux consoles (GPM Mouse mode)"
  :tag "builtin" "mouse")

(leaf gud-tooltip
  :doc "Toggle the display of GUD tooltips"
  :tag "builtin" "debugging")

(leaf header-line-indent
  :doc "Minor mode to help with alignment of header line when line numbers are shown"
  :tag "builtin" "display")

(leaf hexl-follow-ascii
  :doc "Minor mode to follow ASCII in current Hexl buffer"
  :tag "builtin" "hex")

(leaf hi-lock
  :doc "Toggle selective highlighting of patterns (Hi Lock mode)"
  :tag "builtin" "highlighting")

(leaf hide-ifdef
  :doc "Toggle features to hide/show #ifdef blocks (Hide-Ifdef mode)"
  :tag "builtin" "c")

(leaf highlight-changes
  :doc "Toggle highlighting changes in this buffer (Highlight Changes mode)"
  :tag "builtin" "highlighting")

(leaf highlight-changes-visible
  :doc "Toggle visibility of highlighting due to Highlight Changes mode"
  :tag "builtin" "highlighting")

(leaf hl-line
  :doc "Toggle highlighting of the current line (Hl-Line mode)"
  :tag "builtin" "highlighting")

(leaf horizontal-scroll-bar
  :doc "Toggle horizontal scroll bars on all frames (Horizontal Scroll Bar mode)"
  :tag "builtin" "scrolling")

(leaf hs-minor
  :doc "Minor mode to selectively hide/show code and comment blocks"
  :tag "builtin" "folding")

(leaf html-autoview
  :doc "Toggle viewing of HTML files on save (HTML Autoview mode)"
  :tag "builtin" "html")

(leaf icomplete
  :doc "Toggle incremental minibuffer completion (Icomplete mode)"
  :tag "builtin" "completion")

(leaf icomplete-vertical
  :doc "Toggle vertical candidate display in 'icomplete' or 'fido'"
  :tag "builtin" "completion")

(leaf iimage
  :doc "Toggle Iimage mode on or off"
  :tag "builtin" "image")

(leaf image-dired-minor
  :doc "Setup easy-to-use keybindings for Image-Dired in Dired mode"
  :tag "builtin" "image")

(leaf image-minor
  :doc "Toggle Image minor mode in this buffer"
  :tag "builtin" "image")

(leaf indent-tabs
  :doc "Toggle whether indentation can insert TAB characters"
  :tag "builtin" "indentation")

(leaf isearch-fold-quotes
  :doc "Minor mode to aid searching for ` characters in help modes"
  :tag "builtin" "search")

(leaf ispell-minor
  :doc "Toggle last-word spell checking (Ispell minor mode)"
  :tag "builtin" "spell-checking")

(leaf iswitchb
  :doc "Toggle Iswitchb mode"
  :tag "builtin" "buffer-switching")

(leaf jit-lock-debug
  :doc "Minor mode to help debug code run from jit-lock"
  :tag "builtin" "debugging")

(leaf kill-ring-deindent
  :doc "Toggle removal of indentation from text saved to the kill ring"
  :tag "builtin" "editing")

(leaf latex-electric-env-pair
  :doc "Toggle Latex Electric Env Pair mode"
  :tag "builtin" "latex")

(leaf line-number
  :doc "Toggle line number display in the mode line (Line Number mode)"
  :tag "builtin" "mode-line")

(leaf lock-file
  :doc "Toggle file locking in the current buffer (Lock File mode)"
  :tag "builtin" "files")

(leaf lost-selection
  :doc "Toggle 'lost-selection'"
  :tag "builtin" "selection")

(leaf mail-abbrevs
  :doc "Toggle abbrev expansion of mail aliases (Mail Abbrevs mode)"
  :tag "builtin" "mail")

(leaf master
  :doc "Toggle Master mode"
  :tag "builtin" "convenience")

(leaf menu-bar
  :doc "Toggle display of a menu bar on each frame (Menu Bar mode)"
  :tag "builtin" "menu")

(leaf mh-showing
  :doc "Minor mode to show the message in a separate window"
  :tag "builtin" "mail")

(leaf midnight
  :doc "Non-nil means run 'midnight-hook' at midnight"
  :tag "builtin" "convenience")

(leaf minibuffer-depth-indicate
  :doc "Toggle Minibuffer Depth Indication mode"
  :tag "builtin" "minibuffer")

(leaf minibuffer-electric-default
  :doc "Toggle Minibuffer Electric Default mode"
  :tag "builtin" "minibuffer")

(leaf minibuffer-regexp
  :doc "Minor mode for editing regular expressions in the minibuffer"
  :tag "builtin" "regexp")

(leaf mml
  :doc "Minor mode for editing MML"
  :tag "builtin" "mail")

(leaf modifier-bar
  :doc "Toggle display of the modifier bar"
  :tag "builtin" "display")

(leaf mouse-wheel
  :doc "Toggle mouse wheel support (Mouse Wheel mode)"
  :tag "builtin" "mouse"
  :global-minor-mode t
  :custom ((mouse-wheel-scroll-amount . '(3 ((shift) . 5) ((control) . nil)))
           (mouse-wheel-progressive-speed . nil)))

(leaf msb
  :doc "Toggle Msb mode"
  :tag "builtin" "menu")

(leaf next-error-follow-minor
  :doc "Minor mode for compilation, occur and diff modes"
  :tag "builtin" "navigation")

(leaf nroff-electric
  :doc "Toggle automatic nroff request pairing (Nroff Electric mode)"
  :tag "builtin" "nroff")

(leaf org-beamer
  :doc "Support for editing Beamer oriented Org mode files"
  :tag "builtin" "org")

(leaf org-cdlatex
  :doc "Toggle the minor 'org-cdlatex'"
  :tag "builtin" "org")

(leaf org-list-checkbox-radio
  :doc "When turned on, use list checkboxes as radio buttons"
  :tag "builtin" "org")

(leaf org-src
  :doc "Minor mode for language major mode buffers generated by Org"
  :tag "builtin" "org")

(leaf org-table-follow-field
  :doc "Minor mode to make the table field editor window follow the cursor"
  :tag "builtin" "org")

(leaf org-table-header-line
  :doc "Display the first row of the table at point in the header line"
  :tag "builtin" "org")

(leaf orgtbl
  :doc "The Org mode table editor as a minor mode for use in other modes"
  :tag "builtin" "org")

(leaf outline-minor
  :doc "Toggle Outline minor mode"
  :tag "builtin" "outline")

(leaf overwrite
  :doc "Toggle Overwrite mode"
  :tag "builtin" "editing")

(leaf paragraph-indent-minor
  :doc "Minor mode for editing text, with leading spaces starting a paragraph"
  :tag "builtin" "text")

(leaf pascal-outline
  :doc "Outline-line minor mode for Pascal mode"
  :tag "builtin" "pascal")

(leaf pixel-scroll
  :doc "A minor mode to scroll text pixel-by-pixel"
  :tag "builtin" "scrolling")

(leaf pixel-scroll-precision
  :doc "Toggle pixel scrolling"
  :tag "builtin" "scrolling")

(leaf prettify-symbols
  :doc "Toggle Prettify Symbols mode"
  :tag "builtin" "display")

(leaf rcirc-multiline-minor
  :doc "Minor mode for editing multiple lines in rcirc"
  :tag "builtin" "rcirc")

(leaf rcirc-omit
  :doc "Toggle the hiding of \"uninteresting\" lines"
  :tag "builtin" "rcirc")

(leaf rcirc-track-minor
  :doc "Global minor mode for tracking activity in rcirc buffers"
  :tag "builtin" "rcirc")

(leaf read-extended-command
  :doc "Minor mode used for completion in 'read-extended-command'"
  :tag "builtin" "commands")

(leaf read-passwd
  :doc "Toggle visibility of password in minibuffer"
  :tag "builtin" "security")

(leaf recentf
  :doc "Toggle keeping track of opened files (Recentf mode)"
  :tag "builtin" "files"
  :global-minor-mode t
  :custom ((recentf-filename-handlers . '(substring-no-properties))
            recentf-auto-cleanup . 'never))

(leaf rectangle-mark
  :doc "Toggle the region as rectangular"
  :tag "builtin" "editing")

(leaf refill
  :doc "Toggle automatic refilling (Refill mode)"
  :tag "builtin" "text")

(leaf reftex-isearch-minor
  :doc "When on, isearch searches the whole document, not only the current file"
  :tag "builtin" "reftex")

(leaf reftex
  :doc "Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX"
  :tag "builtin" "reftex")

(leaf repeat
  :doc "Toggle Repeat mode"
  :tag "builtin" "convenience")

(leaf reveal
  :doc "Toggle uncloaking of invisible text near point (Reveal mode)"
  :tag "builtin" "display")

(leaf rng-validate
  :doc "Minor mode performing continual validation against a RELAX NG schema"
  :tag "builtin" "xml")

(leaf rst-minor
  :doc "Toggle ReST minor mode"
  :tag "builtin" "rst")

(leaf ruler
  :doc "Toggle display of ruler in header line (Ruler mode)"
  :tag "builtin" "display")

(leaf save-place
  :doc "Non-nil means automatically save place in each file"
  :tag "builtin" "files")

(leaf savehist
  :doc "Toggle saving of minibuffer history (Savehist mode)"
  :tag "builtin" "minibuffer"
  :global-minor-mode t)

(leaf scroll-all
  :doc "Toggle shared scrolling in same-frame windows (Scroll-All mode)"
  :tag "builtin" "scrolling")

(leaf scroll-lock
  :doc "Buffer-local minor mode for pager-like scrolling"
  :tag "builtin" "scrolling")

(leaf semantic-highlight-edits
  :doc "Minor mode for highlighting changes made in a buffer"
  :tag "builtin" "semantic")

(leaf semantic-highlight-func
  :doc "Minor mode to highlight the first line of the current tag"
  :tag "builtin" "semantic")

(leaf semantic
  :doc "Toggle parser features (Semantic mode)"
  :tag "builtin" "semantic")

(leaf semantic-show-parser-state
  :doc "Minor mode for displaying parser cache state in the modeline"
  :tag "builtin" "semantic")

(leaf semantic-show-unmatched-syntax
  :doc "Minor mode to highlight unmatched lexical syntax tokens"
  :tag "builtin" "semantic")

(leaf semantic-stickyfunc
  :doc "Minor mode to show the title of a tag in the header line"
  :tag "builtin" "semantic")

(leaf server
  :doc "Toggle Server mode"
  :tag "builtin" "server")

(leaf sgml-electric-tag-pair
  :doc "Toggle SGML Electric Tag Pair mode"
  :tag "builtin" "sgml")

(leaf sh-electric-here-document
  :doc "Make << insert a here document skeleton"
  :tag "builtin" "shell")

(leaf shell-dirtrack
  :doc "Toggle directory tracking in this shell buffer (Shell Dirtrack mode)"
  :tag "builtin" "shell")

(leaf shell-highlight-undef
  :doc "Highlight undefined shell commands and aliases"
  :tag "builtin" "shell")

(leaf show-paren
  :doc "Toggle visualization of matching parens (Show Paren mode)"
  :tag "builtin" "convenience"
  :global-minor-mode t)

(leaf size-indication
  :doc "Toggle buffer size display in the mode line (Size Indication mode)"
  :tag "builtin" "mode-line")

(leaf smerge
  :doc "Minor mode to simplify editing output from the diff3 program"
  :tag "builtin" "merge")

(leaf so-long-minor
  :doc "This is the minor mode equivalent of 'so-long'"
  :tag "builtin" "performance")

(leaf strokes
  :doc "Toggle Strokes mode, a global minor mode"
  :tag "builtin" "strokes")

(leaf subword
  :doc "Toggle subword movement and editing (Subword mode)"
  :tag "builtin" "editing")

(leaf superword
  :doc "Toggle superword movement and editing (Superword mode)"
  :tag "builtin" "editing")

(leaf tab-bar-history
  :doc "Toggle tab history mode for the tab bar"
  :tag "builtin" "tabs")

(leaf tab-bar
  :doc "Toggle the tab bar in all graphical frames (Tab Bar mode)"
  :tag "builtin" "tabs")

(leaf tab-line
  :doc "Toggle display of tab line in the windows displaying the current buffer"
  :tag "builtin" "tabs")

(leaf table-fixed-width
  :doc "Cell width is fixed when this is non-nil"
  :tag "builtin" "table")

(leaf tar-subfile
  :doc "Minor mode for editing an element of a tar-file"
  :tag "builtin" "tar")

(leaf temp-buffer-resize
  :doc "Toggle auto-resizing temporary buffer windows (Temp Buffer Resize Mode)"
  :tag "builtin" "windows")

(leaf text-scale
  :doc "Minor mode for displaying buffer text in a larger/smaller font"
  :tag "builtin" "text")

(leaf tildify
  :doc "Adds electric behavior to space character"
  :tag "builtin" "text")

(leaf tool-bar
  :doc "Toggle the tool bar in all graphical frames (Tool Bar mode)"
  :tag "builtin" "toolbar")

(leaf tooltip
  :doc "Toggle Tooltip mode"
  :tag "builtin" "tooltip")

(leaf transient-mark
  :doc "Toggle Transient Mark mode"
  :tag "builtin" "editing"
  :global-minor-mode t)

(leaf treesit-explore
  :doc "Enable exploring the current buffer's syntax tree"
  :tag "builtin" "treesit")

(leaf treesit-inspect
  :doc "Minor mode that displays in the mode-line the node which starts at point"
  :tag "builtin" "treesit")

(leaf type-break
  :doc "Enable or disable typing-break mode"
  :tag "builtin" "health")

(leaf type-break-line-message
  :doc "Toggle warnings about typing breaks in the mode line"
  :tag "builtin" "health")

(leaf type-break-query
  :doc "Toggle typing break queries"
  :tag "builtin" "health")

(leaf undelete-frame
  :doc "Enable the 'undelete-frame' command"
  :tag "builtin" "frames")

(leaf url-handler
  :doc "Handle URLs as if they were file names throughout Emacs"
  :tag "builtin" "url")

(leaf vhdl-electric
  :doc "Toggle VHDL electric mode"
  :tag "builtin" "vhdl")

(leaf vhdl-hs-minor
  :doc "Toggle hideshow minor mode and update menu bar"
  :tag "builtin" "vhdl")

(leaf vhdl-stutter
  :doc "Toggle VHDL stuttering mode"
  :tag "builtin" "vhdl")

(leaf view
  :doc "Toggle View mode, a minor mode for viewing text but not editing it"
  :tag "builtin" "viewing")

(leaf viper-harness-minor
  :doc "Familiarize Viper with a minor mode defined in LOAD-FILE"
  :tag "builtin" "viper")

(leaf visible
  :doc "Toggle making all invisible text temporarily visible (Visible mode)"
  :tag "builtin" "display")

(leaf visual-line
  :doc "Toggle visual line based editing (Visual Line mode) in the current buffer"
  :tag "builtin" "text")

(leaf visual-wrap-prefix
  :doc "Display continuation lines with prefixes from surrounding context"
  :tag "builtin" "text")

(leaf which-function
  :doc "Toggle mode line display of current function (Which Function mode)"
  :tag "builtin" "mode-line")

(leaf which-key
  :doc "Toggle 'which-key'"
  :tag "external" "help"
  :global-minor-mode t
  :custom (which-key-idle-delay . 0.3))

(leaf whitespace
  :doc "Toggle whitespace visualization (Whitespace mode)"
  :tag "builtin" "whitespace")

(leaf whitespace-newline
  :doc "Toggle newline visualization (Whitespace Newline mode)"
  :tag "builtin" "whitespace")

(leaf widget-minor
  :doc "Minor mode for traversing widgets"
  :tag "builtin" "widgets")

(leaf windmove
  :doc "Global minor mode for default windmove commands"
  :tag "builtin" "windows")

(leaf window-divider
  :doc "Display dividers between windows (Window Divider mode)"
  :tag "builtin" "windows")

(leaf window-tool-bar
  :doc "Toggle display of the tool bar in the tab line of the current buffer"
  :tag "builtin" "toolbar")

(leaf winner
  :doc "Restore old window configurations"
  :tag "window" "layout" "workspace" "builtin" "navigation"
  :global-minor-mode t)

(leaf xref-etags
  :doc "Minor mode to make xref use etags again"
  :tag "builtin" "navigation")

(leaf xterm-mouse
  :doc "Toggle XTerm mouse mode"
  :tag "builtin" "mouse"
  :unless (display-graphic-p)
  :global-minor-mode t)

;;

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :tag "completion" "ui" "backend"
  :url "https://github.com/minad/vertico"
  :elpaca t
  :global-minor-mode t
  :custom ((vertico-cycle . t)
           (vertico-scroll-margin . 4)))

(leaf vertico-buffer
  :doc "Display Vertico like a regular buffer."
  :tag "completion" "ui" "rendering"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el"
  :after vertico)

(leaf vertico-directory
  :doc "Commands for Ido-like directory navigation."
  :tag "completion" "files" "navigation"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el"
  :after vertico
  :bind ((:vertico-map :package vertico
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("C-DEL" . vertico-directory-delete-word))))

(leaf vertico-flat
  :doc "Enable a flat, horizontal display."
  :tag "completion" "ui" "layout"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-flat.el"
  :after vertico)

(leaf vertico-grid
  :doc "Enable a grid display."
  :tag "completion" "ui" "layout"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-grid.el"
  :after vertico)

(leaf vertico-indexed
  :doc "Select indexed candidates with prefix arguments."
  :tag "completion" "selection" "numeric"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-indexed.el"
  :after vertico)

(leaf vertico-mouse
  :doc "Support mouse for scrolling and candidate selection."
  :tag "completion" "ui" "mouse" "accessibility"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-mouse.el"
  :after vertico
  :hook (vertico-mode-hook . vertico-mouse-mode))

(leaf vertico-multiform
  :doc "Configure Vertico modes per command or completion category."
  :tag "completion" "ui" "config" "routing"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-multiform.el"
  :after vertico)

(leaf vertico-quick
  :doc "Commands to select using Avy-style quick keys."
  :tag "completion" "ui" "selection" "keyboard"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el"
  :after vertico)

(leaf vertico-repeat
  :doc "Repeats the last completion session."
  :tag "completion" "session" "persistence"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el"
  :after vertico)

(leaf vertico-reverse
  :doc "Reverse the display."
  :tag "completion" "ui" "layout"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-reverse.el"
  :after vertico)

(leaf vertico-suspend
  :doc "Suspends and restores the current session."
  :tag "completion" "session" "control"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-suspend.el"
  :after vertico)

(leaf vertico-unobtrusive
  :doc "Displays only the topmost candidate."
  :tag "completion" "minimal" "ui"
  :url "https://github.com/minad/vertico/blob/main/extensions/vertico-unobtrusive.el"
  :after vertico)

;;

(leaf marginalia
  :doc "Marginalia in the minibuffer"
  :tag "completion" "ui" "annotation"
  :url "https://github.com/minad/marginalia"
  :elpaca t
  :global-minor-mode t)

(leaf orderless
  :doc "Emacs completion style that matches multiple regexps in any order."
  :tag "completion" "style" "matching" "regex"
  :url "https://github.com/oantolin/orderless"
  :elpaca t
  :custom ((completion-styles . '(orderless basic))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf consult
  :doc "consult.el - Consulting completing-read"
  :tag "completion" "ui" "command" "search" "navigation"
  :url "https://github.com/minad/consult"
  :elpaca t)

(leaf embark
  :doc "Emacs Mini-Buffer Actions Rooted in Keymaps."
  :tag "minibuffer" "completion" "convenience"
  :url "https://github.com/oantolin/embark"
  :elpaca t)

(leaf embark-consult
  :doc "Emacs Mini-Buffer Actions Rooted in Keymaps."
  :tag "minibuffer" "completion" "convenience"
  :url "https://github.com/oantolin/embark"
  :elpaca t)

(leaf cape
  :doc "Completion At Point Extensions."
  :url "https://github.com/minad/cape"
  :tag "completion", "editing" "extensions"
  :elpaca t)

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :tag "text" "completion" "matching" "convenience"
  :url "https://github.com/minad/corfu"
  :elpaca t)

(leaf corfu-mouse
  :doc "Mouse support for Corfu completion"
  :tag "completion" "ui" "mouse" "convenience"
  :url "https://codeberg.org/materus/emacs-corfu-mouse"
  :elpaca (corfu-mouse :host codeberg :repo "materus/emacs-corfu-mouse"))

;;

(leaf magit
  :doc "It's Magit! A Git porcelain inside Emacs."
  :tag "vcs" "git" "tooling" "interface"
  :url "https://github.com/magit/magit"
  :elpaca t
  :preface (defun magit-display-buffer-same-window (buffer)
             "Display BUFFER in the selected window."
             (display-buffer
              buffer '(display-buffer-same-window)))
  :custom (magit-bury-buffer-function . #'magit-restore-window-configuration)
          (magit-display-buffer-function . 'magit-display-buffer-same-window)
          (magit-popup-display-buffer-action . '((display-buffer-same-window)))
          (magit-wip-mode . t)
          (magit-save-repository-buffers . nil))

(leaf forge
  :doc "Work with Git forges from the comfort of Magit"
  :tag "vcs" "git" "integration"
  :url "https://github.com/magit/forge"
  :elpaca t
  :custom ((github.user . "wroyca")
           (auth-sources . '("~/.config/forge/.github-authinfo"))))

(leaf transient
  :doc "Transient commands"
  :tag "ui" "keybinding" "command" "infrastructure"
  :url "https://github.com/magit/transient"
  :elpaca t)

;;

(leaf xclip
  :doc "Copy&paste GUI clipboard from terminal Emacs"
  :tag "clipboard" "integration" "ux" "external"
  :url "https://github.com/emacsmirror/xclip"
  :elpaca t
  :global-minor-mode t)

(leaf kkp
  :doc "Emacs support for the Kitty Keyboard Protocol"
  :tag "internal"
  :url "https://github.com/benotn/kkp"
  :elpaca t
  :unless (display-graphic-p)
  :global-minor-mode global-kkp-mode)

(leaf gcmh
  :doc "The GNU Emacs Garbage Collector Magic Hack"
  :tag "internal"
  :url "https://gitlab.com/koral/gcmh"
  :elpaca t
  :global-minor-mode t)

(leaf hide-comnt
  :doc "Hide/show comments in code."
  :tag "comment" "hide" "show"
  :url "https://www.emacswiki.org/emacs/download/hide-comnt.el"
  :elpaca (hide-comnt :host github :repo "emacsmirror/hide-comnt"))

;;

;; The problem we are solving here is in some sense mundane, yet
;; annoyingly subtle: making prefix keymaps both self-documenting
;; and properly integrated with `which-key` when the key in question
;; comes from the keypad (or any other less conventional source).
;;
;; By default, Emacs keymap definitions are only half the story:
;; one can bind keys to functions, but the descriptions shown in
;; `which-key` (and other helpers) require an extra, parallel set
;; of declarations. This redundancy is tolerable when you have a
;; handful of bindings, but once you start introducing structured
;; prefix keymaps (e.g., "File", "Buffers", "Windows"), the boilerplate
;; multiplies quickly. Worse still, consistency between what's bound
;; and what's shown becomes fragile: update one without the other and
;; you've invited confusion.
;;
;; There is also the keypad wrinkle: Emacs treats keypad keys as
;; distinct from their regular counterparts (e.g., `kp-1` versus `1`),
;; yet `which-key` doesn't really care and would happily show nothing
;; or something misleading. Out of the box, there's no clean way to
;; say "bind this here, and describe it there, and do so recursively
;; for nested prefixes."
;;
;; The result is this macro, which may look monstrous at first sight,
;; but whose monstrosity is deliberate: we take on complexity once,
;; centrally, so that we can write succinct, declarative
;; prefix specifications thereafter.

(defmacro define-prefix-keymap (var-name prefix description &rest bindings)
  "Define a prefix keymap VAR-NAME bound to PREFIX with DESCRIPTION shown in which-key.
BINDINGS is a list of (key function description) or (key nested-description &rest nested-bindings)."
  (declare (indent defun))
  (let ((map-name (make-symbol "map")))
    `(progn
       (defvar ,var-name nil ,(format "Keymap for %s" description))
       (let ((,map-name (make-sparse-keymap)))
         ,@(cl-loop for binding in bindings
             collect
             (pcase binding
               (`(,key ,func ,desc)
                 `(progn
                    (define-key ,map-name (kbd ,key) ,func)
                    (let ((full-key (concat ,prefix " " ,key)))
                      (with-eval-after-load 'which-key
                        (when (symbolp ,func)
                          (push (cons (cons nil (symbol-name ,func))
                                  (cons nil ,desc))
                            which-key-replacement-alist))
                        (which-key-add-key-based-replacements full-key ,desc)))))
               (`(,key ,nested-desc . ,nested-bindings)
                 (let ((nested-var (intern (format "%s-%s-keymap"
                                             (symbol-name var-name)
                                             (replace-regexp-in-string " " "-" key)))))
                   `(progn
                      (define-prefix-keymap ,nested-var
                        ,(concat prefix " " key) ,nested-desc
                        ,@nested-bindings)
                      (define-key ,map-name (kbd ,key) ,nested-var))))))
         (setq ,var-name ,map-name)
         (global-set-key (kbd ,prefix) ,var-name)
         (with-eval-after-load 'which-key
           (which-key-add-key-based-replacements ,prefix ,description)
           (which-key-add-keymap-based-replacements global-map ,prefix ,description)
           (push (cons (cons nil (symbol-name ',var-name))
                   (cons nil ,description))
             which-key-replacement-alist))
         ,var-name))))

(define-prefix-keymap dotemacs-consult-buffer-keymap "C-c c b" "Buffers"
  ("b" #'consult-buffer "Switch buffer")
  ("w" #'consult-buffer-other-window "Switch buffer (other window)")
  ("f" #'consult-buffer-other-frame "Switch buffer (other frame)")
  ("t" #'consult-buffer-other-tab "Switch buffer (other tab)")
  ("p" #'consult-project-buffer "Switch project buffer")
  ("k" #'consult-bookmark "Jump to bookmark")
  ("r" #'consult-recent-file "Visit recent file"))
(define-prefix-keymap dotemacs-consult-editing-keymap "C-c c e" "Editing"
  ("y" #'consult-yank-from-kill-ring "Yank from kill ring")
  ("p" #'consult-yank-pop "Yank pop")
  ("r" #'consult-yank-replace "Yank replace")
  ("k" #'consult-kmacro "Call keyboard macro"))
(define-prefix-keymap dotemacs-consult-register-keymap "C-c c r" "Registers"
  ("r" #'consult-register "Insert register")
  ("l" #'consult-register-load "Load register")
  ("s" #'consult-register-store "Store register"))
(define-prefix-keymap dotemacs-consult-navigation-keymap "C-c c n" "Navigation"
  ("g" #'consult-goto-line "Go to line")
  ("m" #'consult-mark "Jump to mark")
  ("M" #'consult-global-mark "Jump to global mark")
  ("o" #'consult-outline "Jump to outline")
  ("i" #'consult-imenu "Jump via imenu")
  ("I" #'consult-imenu-multi "Jump via imenu (multi-buffer)"))
(define-prefix-keymap dotemacs-consult-search-keymap "C-c c s" "Search"
  ("l" #'consult-line "Search line")
  ("L" #'consult-line-multi "Search line (multi-buffer)")
  ("k" #'consult-keep-lines "Keep matching lines")
  ("f" #'consult-focus-lines "Focus matching lines"))
(define-prefix-keymap dotemacs-consult-grep-keymap "C-c c g" "Grep & find"
  ("g" #'consult-grep "Search with grep")
  ("r" #'consult-ripgrep "Search with ripgrep")
  ("G" #'consult-git-grep "Search with git-grep")
  ("f" #'consult-find "Find file")
  ("d" #'consult-fd "Find file with fd")
  ("l" #'consult-locate "Locate file"))
(define-prefix-keymap dotemacs-consult-compilation-keymap "C-c c c" "Diagnostics"
  ("c" #'consult-compile-error "Jump to compile error")
  ("f" #'consult-flymake "List Flymake diagnostics")
  ("x" #'consult-xref "Find references (xref)"))
(define-prefix-keymap dotemacs-consult-history-keymap "C-c c h" "Histories"
  ("c" #'consult-complex-command "Repeat complex command")
  ("h" #'consult-history "Repeat command")
  ("i" #'consult-isearch-history "Repeat isearch"))
(define-prefix-keymap dotemacs-consult-modes-keymap "C-c c m" "Modes"
  ("m" #'consult-minor-mode-menu "Minor mode menu")
  ("c" #'consult-mode-command "Major mode commands"))
(define-prefix-keymap dotemacs-consult-org-keymap "C-c c o" "Org mode"
  ("h" #'consult-org-heading "Jump to heading")
  ("a" #'consult-org-agenda "Open agenda"))
(define-prefix-keymap dotemacs-consult-help-keymap "C-c c H" "Help"
  ("m" #'consult-man "Display man page")
  ("i" #'consult-info "Display info manual"))
(define-prefix-keymap dotemacs-consult-misc-keymap "C-c c M" "Miscellaneous"
  ("t" #'consult-theme "Load theme")
  ("p" #'consult-preview-at-point "Preview at point")
  ("c" #'consult-completion-in-region "Completion in region"))
(define-prefix-keymap dotemacs-consult-keymap "C-c c" "Consult"
  ("b" dotemacs-consult-buffer-keymap "Buffers")
  ("e" dotemacs-consult-editing-keymap "Editing")
  ("r" dotemacs-consult-register-keymap "Registers")
  ("n" dotemacs-consult-navigation-keymap "Navigation")
  ("s" dotemacs-consult-search-keymap "Search")
  ("g" dotemacs-consult-grep-keymap "Grep & find")
  ("c" dotemacs-consult-compilation-keymap "Diagnostics")
  ("h" dotemacs-consult-history-keymap "Histories")
  ("m" dotemacs-consult-modes-keymap "Modes")
  ("o" dotemacs-consult-org-keymap  "Org mode")
  ("H" dotemacs-consult-help-keymap "Help")
  ("M" dotemacs-consult-misc-keymap "Miscellaneous"))

;;

(define-prefix-keymap dotemacs-magit-status-keymap "C-c g s" "Status"
  ("s" #'magit-status "Open Magit status")
  ("l" #'magit-log "Show repository log")
  ("c" #'magit-commit "Commit changes")
  ("S" #'magit-stage-file "Stage file")
  ("u" #'magit-unstage-file "Unstage file"))
(define-prefix-keymap dotemacs-magit-branch-keymap "C-c g b" "Branch"
  ("b" #'magit-branch "Manage branches")
  ("c" #'magit-checkout "Checkout branch")
  ("n" #'magit-branch-and-checkout "Create and checkout branch"))
(define-prefix-keymap dotemacs-magit-remote-keymap "C-c g r" "Remote"
  ("f" #'magit-fetch "Fetch from remote")
  ("p" #'magit-push-current "Push current branch")
  ("P" #'magit-pull "Pull changes")
  ("R" #'magit-remote "Manage remotes"))
(define-prefix-keymap dotemacs-magit-stash-keymap "C-c g z" "Stash"
  ("s" #'magit-stash "Stash")
  ("p" #'magit-stash-pop "Pop stash")
  ("l" #'magit-stash-list "List stashes"))
(define-prefix-keymap dotemacs-magit-keymap "C-c g" "Magit"
  ("s" dotemacs-magit-status-keymap "Status")
  ("b" dotemacs-magit-branch-keymap "Branch")
  ("r" dotemacs-magit-remote-keymap "Remote")
  ("z" dotemacs-magit-stash-keymap "Stash")
  ("d" #'magit-dispatch "Dispatch"))

;;

;; Remap C-x f to find-file to handle "fat-finger" errors.
;;
;; We frequently miss the Control modifier on the second keystroke when
;; intending to type C-x C-f. Since the default binding for C-x f
;; (set-fill-column) is rarely used interactively, it is safe for us to
;; override it. This saves us the frustration of accidentally triggering
;; column formatting when we just wanted to open a file.
;;
(global-set-key (kbd "C-x f") #'find-file)

;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'dotemacs-persistent-regions)

;;

(provide 'init)
