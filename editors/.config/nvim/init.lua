local lazy = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazy) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazy,
  })
end

vim.g.mapleader = ' '

vim.api.nvim_set_keymap('n', '<Space>',   '<Nop>',                       { noremap = true, silent = true }) -- prevent mapleader from triggering unintended actions.
vim.api.nvim_set_keymap('n', '<Up>',      '<cmd>vsp<cr>',                { noremap = true })                -- split window vertically and open new file above
vim.api.nvim_set_keymap('n', '<Down>',    '<cmd>sp<cr>',                 { noremap = true })                -- split window horizontally and open new file below
vim.api.nvim_set_keymap('n', '<Left>',    '<cmd>vsp<cr>',                { noremap = true })                -- split window vertically and open new file to the left
vim.api.nvim_set_keymap('n', '<Right>',   '<cmd>sp<cr>',                 { noremap = true })                -- split window horizontally and open new file to the right
vim.api.nvim_set_keymap('n', '<C-Up>',    '<cmd>resize -2<CR>',          { noremap = true })                -- decrease window height by 2 rows
vim.api.nvim_set_keymap('n', '<C-Down>',  '<cmd>resize +2<CR>',          { noremap = true })                -- increase window height by 2 rows
vim.api.nvim_set_keymap('n', '<C-Left>',  '<cmd>vertical resize -2<CR>', { noremap = true })                -- decrease window width by 2 columns
vim.api.nvim_set_keymap('n', '<C-Right>', '<cmd>vertical resize +2<CR>', { noremap = true })                -- increase window width by 2 columns
vim.api.nvim_set_keymap('i', '<esc>',     '<cmd>noh<cr><esc>',           { noremap = true, silent = true }) -- disable search highlight and exit insert mode or cancel any pending command in normal mode.
vim.api.nvim_set_keymap('n', '<esc>',     '<cmd>noh<cr><esc>',           { noremap = true, silent = true }) -- ^ 
vim.api.nvim_set_keymap('n', '<C-b>',     ':NvimTreeToggle<CR>',         { noremap = true, silent = true }) -- toggle NvimTree file explorer
vim.api.nvim_set_keymap('n', '<C-h>',     ':HopWord<CR>',                { noremap = true, silent = true }) -- use HopWord to jump between words quickly

vim.opt.cursorline     = true                                          -- highlight the line the cursor is on
vim.opt.swapfile       = false                                         -- disable swapfile creation
vim.opt.autowrite      = true                                          -- automatically save the buffer before executing certain commands
vim.opt.autowriteall   = true                                          -- automatically save all modified buffers when exiting Vim
vim.opt.cdhome         = true                                          -- change the current directory to the user's home directory
vim.opt.relativenumber = true                                          -- show line numbers relative to the current line
vim.opt.laststatus     = 0                                             -- disable the status line in inactive windows
vim.opt.encoding       = 'utf-8'                                       -- set the character encoding to UTF-8
vim.opt.fileencoding   = 'utf-8'                                       -- set the file encoding to UTF-8
vim.scriptencoding     = 'utf-8'                                       -- set the script encoding to UTF-8
vim.opt.mouse          = 'a'                                           -- enable mouse support in all modes
vim.opt.backup         = false                                         -- disable backup file creation
vim.opt.writebackup    = false                                         -- disable backup file creation when overwriting a file
vim.opt.undofile       = true                                          -- enable persistent undo
vim.opt.undodir        = vim.fn.expand('$HOME/.config/nvim/undo')      -- set the directory for persistent undo files
vim.opt.termguicolors  = true                                          -- enable true colors in the terminal
vim.opt.number         = true                                          -- show line numbers
vim.opt.signcolumn     = 'yes'                                         -- always show the sign column, even when there are no signs
vim.opt.pumheight      = 10                                            -- set the height of the pop-up menu
vim.opt.splitbelow     = true                                          -- split new windows below the current window
vim.opt.splitright     = true                                          -- split new windows to the right of the current window
vim.opt.showmode       = false                                         -- do not show the current mode (e.g., Insert) in the command line
vim.opt.cmdheight      = 1                                             -- set the height of the command line
vim.opt.expandtab      = true                                          -- expand tabs to spaces
vim.opt.tabstop        = 2                                             -- set the width of a tab character to 2 spaces
vim.opt.smarttab       = true                                          -- use the appropriate number of spaces when using <Tab> to indent
vim.opt.shiftwidth     = 2                                             -- set the width of an indentation level to 2 spaces
vim.opt.smartindent    = true                                          -- enable smart indentation
vim.opt.autoindent     = true                                          -- automatically indent new lines to the same level as the previous line
vim.opt.breakindent    = true                                          -- preserve indentation when wrapping lines
vim.opt.virtualedit    = 'block'                                       -- enable blockwise visual mode
vim.opt.completeopt    = { 'menu', 'noinsert', 'noselect', 'preview' } -- set options for insert mode completion
vim.opt.clipboard      = "unnamedplus"                                 -- use the system clipboard
vim.opt.shortmess      = vim.opt.shortmess + 'A'                       -- ignore annoying swapfile messages
vim.opt.shortmess      = vim.opt.shortmess + 'I'                       -- no splash screen
vim.opt.shortmess      = vim.opt.shortmess + 'O'                       -- file-read message overwrites previous
vim.opt.shortmess      = vim.opt.shortmess + 'T'                       -- truncate non-file messages in middle
vim.opt.shortmess      = vim.opt.shortmess + 'W'                       -- don't echo "[w]"/"[written]" when writing
vim.opt.shortmess      = vim.opt.shortmess + 'a'                       -- use abbreviations in messages eg. `[RO]` instead of `[readonly]`
vim.opt.shortmess      = vim.opt.shortmess + 'c'                       -- completion messages
vim.opt.shortmess      = vim.opt.shortmess + 'o'                       -- overwrite file-written messages
vim.opt.shortmess      = vim.opt.shortmess + 't'                       -- truncate file messages at start
vim.opt.formatoptions:append( 'q' )                                    -- allow formatting comments with "gq"
vim.opt.formatoptions:remove( 't' )                                    -- don't auto-wrap text
vim.opt.formatoptions:append( 'c' )                                    -- auto-wrap comments using textwidth, inserting the current comment leader automatically
vim.opt.formatoptions:append( 'j' )                                    -- where it makes sense, remove a comment leader when joining lines
vim.opt.formatoptions:append( 'p' )                                    -- don't break lines at single spaces that follow periods
vim.opt.formatoptions:append( '1' )                                    -- don't break a line after a one-letter word (do it before)
vim.opt.formatoptions:remove( 'l' )                                    -- long lines are broken in insert mode
vim.opt.formatoptions:remove( 'o' )                                    -- don't automatically insert the current comment leader after hitting 'o' or 'O' in Normal mode
vim.opt.formatoptions:append( 'r' )                                    -- automatically insert the current comment leader after hitting <Enter> in Insert mode
vim.opt.rtp:prepend(lazy)

-- Automatically move the cursor to the last position when opening a file.
-- 
-- This function is an autocmd callback that is triggered on `BufWinEnter` 
-- and `FileType` events. It checks if the buffer type and filetype are not 
-- ignored, and if the cursor is at the beginning of the file.
--
-- If the cursor is not at the beginning of the file, or the buffer type or 
-- filetype is ignored, this function does nothing. Otherwise, it moves the 
-- cursor to the last position where it was before the buffer was last closed, 
-- or to the center of the screen if it was not previously opened.
--
local function run()
  -- Check if buffer type is ignored
  if vim.tbl_contains({ "quickfix", "nofile", "help" }, vim.bo.buftype) then
    return
  end

  -- Check if filetype is ignored
  if vim.tbl_contains({ "gitcommit", "gitrebase", "svn", "hgcommit" }, vim.bo.filetype) then
    vim.cmd[[normal! gg]]
    return
  end

  -- Check if cursor is at beginning of file
  if vim.fn.line(".") > 1 then
    return
  end

  -- Move cursor to last position or center of screen
  local last_line = vim.fn.line([['"]])
  local buff_last_line = vim.fn.line("$")

  if last_line > 0 and last_line <= buff_last_line then
    local win_last_line = vim.fn.line("w$")
    local win_first_line = vim.fn.line("w0")

    if win_last_line == buff_last_line then
      vim.cmd[[normal! g`"]]
    elseif buff_last_line - last_line > ((win_last_line - win_first_line) / 2) - 1 then
      vim.cmd[[normal! g`"zz]]
    else
      vim.cmd[[normal! G'"<c-e>]]
    end
  end
end

vim.api.nvim_create_autocmd({'BufWinEnter', 'FileType'}, {
  group = vim.api.nvim_create_augroup('nvim-lastplace', {}),
  callback = run
})

-- With lazy.nvim library, we can avoid passing a spec table to the setup() 
-- function by creating a separate Lua module. The module's specs, along with
-- any top-level sub-modules, will be merged automatically into the final spec,
-- eliminating the need to add require() calls to the other files in our main 
-- plugin file.
--
require("lazy").setup("specs")
