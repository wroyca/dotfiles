vim.g.mapleader = ' '

local lazy = {}
local lazy_directory = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

-- The specs from the module and any top-level sub-modules should be merged 
-- together in the final spec, so it should not needed to add require calls in 
-- our main plugin file to the other files. Unfortunately, this doesn't appears
-- to work reliably, so we default to a spec table for convenience. 

table.insert(lazy, require("specs.autopairs"))
table.insert(lazy, require("specs.cmp"))
table.insert(lazy, require("specs.colorscheme"))
table.insert(lazy, require("specs.devicons"))
table.insert(lazy, require("specs.editorconfig"))
table.insert(lazy, require("specs.hop"))
table.insert(lazy, require("specs.indent"))
table.insert(lazy, require("specs.lsp"))
table.insert(lazy, require("specs.lspkind"))
table.insert(lazy, require("specs.telescope"))
table.insert(lazy, require("specs.tree"))
table.insert(lazy, require("specs.treesitter"))

vim.api.nvim_set_keymap('n', '<C-b>', ':NvimTreeToggle<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<C-h>', ':HopWord<CR>', {noremap = true, silent = true})

if not vim.loop.fs_stat(lazy_directory) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazy_directory})
end

vim.opt.shortmess      = vim.opt.shortmess + 'A' -- ignore annoying swapfile messages
vim.opt.shortmess      = vim.opt.shortmess + 'I' -- no splash screen
vim.opt.shortmess      = vim.opt.shortmess + 'O' -- file-read message overwrites previous
vim.opt.shortmess      = vim.opt.shortmess + 'T' -- truncate non-file messages in middle
vim.opt.shortmess      = vim.opt.shortmess + 'W' -- don't echo "[w]"/"[written]" when writing
vim.opt.shortmess      = vim.opt.shortmess + 'a' -- use abbreviations in messages eg. `[RO]` instead of `[readonly]`
vim.opt.shortmess      = vim.opt.shortmess + 'c' -- completion messages
vim.opt.shortmess      = vim.opt.shortmess + 'o' -- overwrite file-written messages
vim.opt.shortmess      = vim.opt.shortmess + 't' -- truncate file messages at start
vim.opt.autowrite      = true
vim.opt.autowriteall   = true
vim.opt.cdhome         = true
vim.opt.relativenumber = true
vim.opt.laststatus     = 0
vim.opt.encoding       = 'utf-8'
vim.opt.fileencoding   = 'utf-8'
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
vim.opt.rtp:prepend(lazy_directory)

--  Automatically restore the cursor at our last position.
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

-- Automatically set up our configuration after cloning lazy.nvim
-- This must be added at the end.
require("lazy").setup(lazy, {
  rtp = {
    reset = true,
    disabled_plugins = {
      "netrw",
      "netrwPlugin",
      "netrwSettings",
      "netrwFileHandlers",
      "gzip",
      "zip",
      "zipPlugin",
      "tar",
      "tarPlugin",
      "getscript",
      "getscriptPlugin",
      "vimball",
      "vimballPlugin",
      "2html_plugin",
      "logipat",
      "rrhelper",
      "spellfile_plugin",
      "matchit"
    },
  },
})
