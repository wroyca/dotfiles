local lazy = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazy) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazy})
end

-- Disable F1 built-in help key
vim.keymap.set('n', '<F1>', '<Nop>')

-- Fix vim leader
vim.g.mapleader = ' '
vim.keymap.set('n', '<Space>', '<Nop>')

-- Fix vim line motions
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Fix vim indent
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- Fix vim search
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Fix vim search highlight
vim.keymap.set({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>")

-- Fix vim split resize
vim.keymap.set("n",    "<C-Up>",  "<cmd>resize -2<CR>")
vim.keymap.set("n",  "<C-Down>",  "<cmd>resize +2<CR>")
vim.keymap.set("n",  "<C-Left>",  "<cmd>vertical resize -2<CR>")
vim.keymap.set("n", "<C-Right>",  "<cmd>vertical resize +2<CR>")

-- Plugins
vim.api.nvim_set_keymap('n', '<C-b>', ':NvimTreeToggle<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<C-h>', ':HopWord<CR>', {noremap = true, silent = true})

vim.opt.cursorline     = true
vim.opt.swapfile       = false
vim.opt.autowrite      = true
vim.opt.autowriteall   = true
vim.opt.cdhome         = true
vim.opt.relativenumber = true
vim.opt.laststatus     = 0
vim.opt.encoding       = 'utf-8'
vim.opt.fileencoding   = 'utf-8'
vim.scriptencoding     = 'utf-8'
vim.opt.mouse          = 'a'
vim.opt.backup         = false
vim.opt.writebackup    = false
vim.opt.undofile       = true
vim.opt.undodir        = vim.fn.expand('$HOME/.config/nvim/undo')
vim.opt.termguicolors  = true
vim.opt.number         = true
vim.opt.signcolumn     = 'yes'
vim.opt.pumheight      = 10
vim.opt.splitbelow     = true
vim.opt.splitright     = true
vim.opt.showmode       = false
vim.opt.cmdheight      = 1
vim.opt.expandtab      = true
vim.opt.tabstop        = 2
vim.opt.smarttab       = true
vim.opt.shiftwidth     = 2
vim.opt.smartindent    = true
vim.opt.autoindent     = true
vim.opt.breakindent    = true
vim.opt.virtualedit    = 'block'
vim.opt.completeopt    = { 'menu', 'noinsert', 'noselect', 'preview' }
vim.opt.clipboard      = "unnamedplus"
vim.opt.shortmess      = vim.opt.shortmess + 'A' -- ignore annoying swapfile messages
vim.opt.shortmess      = vim.opt.shortmess + 'I' -- no splash screen
vim.opt.shortmess      = vim.opt.shortmess + 'O' -- file-read message overwrites previous
vim.opt.shortmess      = vim.opt.shortmess + 'T' -- truncate non-file messages in middle
vim.opt.shortmess      = vim.opt.shortmess + 'W' -- don't echo "[w]"/"[written]" when writing
vim.opt.shortmess      = vim.opt.shortmess + 'a' -- use abbreviations in messages eg. `[RO]` instead of `[readonly]`
vim.opt.shortmess      = vim.opt.shortmess + 'c' -- completion messages
vim.opt.shortmess      = vim.opt.shortmess + 'o' -- overwrite file-written messages
vim.opt.shortmess      = vim.opt.shortmess + 't' -- truncate file messages at start
vim.opt.formatoptions:append( 'q' ) -- Allow formatting comments with "gq".
vim.opt.formatoptions:remove( 't' ) -- Don't auto-wrap text.
vim.opt.formatoptions:append( 'c' ) -- Auto-wrap comments using textwidth, inserting the current comment leader automatically.
vim.opt.formatoptions:append( 'j' ) -- Where it makes sense, remove a comment leader when joining lines.
vim.opt.formatoptions:append( 'p' ) -- Don't break lines at single spaces that follow periods.
vim.opt.formatoptions:append( '1' ) -- Don't break a line after a one-letter word (do it before).
vim.opt.formatoptions:remove( 'l' ) -- Long lines are broken in insert mode.
vim.opt.formatoptions:remove( 'o' ) -- Don't automatically insert the current comment leader after hitting 'o' or 'O' in Normal mode.
vim.opt.formatoptions:append( 'r' ) -- Automatically insert the current comment leader after hitting <Enter> in Insert mode.
vim.opt.rtp:prepend(lazy)

-- Remember and restore the cursor at our last position.
local ignore_buftype = { "quickfix", "nofile", "help" }
local ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit" }
local function run()
  if vim.tbl_contains(ignore_buftype, vim.bo.buftype) then
    return
  end
  if vim.tbl_contains(ignore_filetype, vim.bo.filetype) then
    vim.cmd[[normal! gg]]
    return
  end
  if vim.fn.line(".") > 1 then
    return
  end
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

-- Instead of passing a spec table to setup(), we use a Lua module. The specs
-- from the module and any top-level sub-modules will be merged together in
-- the final spec, so it is not needed to add require calls in our main plugin file to the other files.
require("lazy").setup("specs")

