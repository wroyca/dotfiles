local _ = string.format([[
     .-.      _______                             .  '  *   .  . '
    {}``; |==|_______D                                  . *  -+-  .
    / ('        /|\                                 . '   * .    '  *
(  /  |        / | \                                    * .  ' .  .-+-
 \(_)_%s      /  |  \                                *   *  .   .]], "]]")

local h = io.popen([[
gdbus call --session \
           --dest=org.freedesktop.portal.Desktop \
           --object-path=/org/freedesktop/portal/desktop \
           --method=org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme
]])

if h ~= nil then
  if string.match(h:read('*a'), ' %d') == " 1" then
    h:close()
    vim.api.nvim_exec2([[colorscheme dark]], {})
  else
    h:close()
    vim.api.nvim_exec2([[colorscheme light]], {})
  end
end

if vim.fn.expand([[$TERM]]) == [[xterm-kitty]] and not vim.g.neovide then
  vim.api.nvim_exec2([[set guicursor=n-v-c-sm:block-Cursor,i-ci-ve:ver25-Cursor,r-cr-o:hor20-Cursor]], {})
end

local shada = vim.o.shada
vim.o.shada = [[]]
vim.api.nvim_create_autocmd([[User]], {
  pattern = [[VeryLazy]],
  callback = function()
    vim.o.shada = shada
    pcall(vim.cmd.rshada, { bang = true })
  end
})

vim.g.mapleader      = [[ ]]
vim.g.localmapleader = [[,]]
vim.o.clipboard      = [[unnamedplus]]
vim.o.fileencoding   = [[utf-8]]
vim.o.whichwrap      = [[b,s,<,>,h,l]]
vim.o.signcolumn     = [[yes:1]]
vim.o.virtualedit    = [[onemore]]
vim.o.titlestring    = [[%t]]
vim.o.splitkeep      = [[screen]]
vim.o.showmode       = false
vim.o.number         = false
vim.o.relativenumber = false
vim.wo.wrap          = true
vim.o.fsync          = true
vim.o.title          = true
vim.o.confirm        = true
vim.o.splitbelow     = true
vim.o.splitright     = true
vim.o.termguicolors  = true
vim.o.undofile       = true
vim.o.expandtab      = true
vim.o.autoindent     = true
vim.o.breakindent    = true
vim.o.smartindent    = true
vim.o.smarttab       = true
vim.o.preserveindent = true
vim.o.cursorline     = true
vim.o.conceallevel   = 3
vim.o.winblend       = 0
vim.o.tabstop        = 2
vim.o.shiftwidth     = 2
vim.o.scrolloff      = 4
vim.o.pumheight      = 5
vim.o.cmdheight      = 1
vim.o.synmaxcol      = 0
vim.o.laststatus     = 0
vim.o.foldlevel      = 99
vim.o.foldlevelstart = 99
vim.o.foldenable     = true
vim.o.list           = true
vim.opt.suffixes     = vim.opt.suffixes - [[.h]]
vim.opt.cinkeys      : remove [[:]]
vim.opt.indentkeys   : remove [[:]]
vim.opt.shortmess    : append [[sI]]

local lazypath = vim.fn.stdpath [[data]] .. [[/lazy/lazy.nvim]]
if not vim.uv.fs_stat(lazypath) then
  vim.system({
    [[git]],
    [[clone]],
    [[--filter=blob:none]],
    [[https://github.com/folke/lazy.nvim.git]],
    [[--branch=stable]], -- latest stable release
    lazypath
  }, { text = true }):wait()
end
vim.opt.rtp:prepend(lazypath)

require [[cfg.keys]].setup()
require [[cfg.cmds]].setup()
require [[lazy]].setup([[spec]], {
  performance = {
    rtp = {
      disabled_plugins = {
        [[2html_plugin]],
        [[bugreport]],
        [[compiler]],
        [[ftplugin]],
        [[getscript]],
        [[getscriptPlugin]],
        [[gzip]],
        [[health]],
        [[logipat]],
        [[matchit]],
        [[netrw]],
        [[netrwFileHandlers]],
        [[netrwPlugin]],
        [[netrwSettings]],
        [[nvim]],
        [[optwin]],
        [[rplugin]],
        [[rrhelper]],
        [[shada]],
        [[spellfile]],
        [[spellfile_plugin]],
        [[synmenu]],
        [[syntax]],
        [[tar]],
        [[tarPlugin]],
        [[tohtml]],
        [[tutor]],
        [[vimball]],
        [[vimballPlugin]],
        [[zip]],
        [[zipPlugin]]
      }
    }
  },

  defaults = {
    lazy = true,
    version = false
  },

  change_detection = {
    enabled = false
  }
})
