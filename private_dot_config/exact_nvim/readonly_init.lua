local _ = string.format([[
     .-.      _______                             .  '  *   .  . '
    {}``; |==|_______D                                  . *  -+-  .
    / ('        /|\                                 . '   * .    '  *
(  /  |        / | \                                    * .  ' .  .-+-
 \(_)_%s      /  |  \                                *   *  .   .]], "]]")

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
vim.o.cmdheight      = 0
vim.o.synmaxcol      = 0
vim.o.laststatus     = 0
vim.o.foldlevel      = 99
vim.o.foldlevelstart = 99
vim.o.foldenable     = true
vim.o.list           = true
vim.opt.suffixes     = vim.opt.suffixes - [[.h]]
vim.opt.cinkeys      : remove([[:]])
vim.opt.indentkeys   : remove([[:]])

local lazypath = vim.fn.stdpath([[data]]) .. [[/lazy/lazy.nvim]]
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    [[git]],
    [[clone]],
    [[--filter=blob:none]],
    [[https://github.com/folke/lazy.nvim.git]],
    [[--branch=stable]], -- latest stable release
    lazypath
  })
end
vim.opt.rtp:prepend(lazypath)

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

  install = {
    colorscheme = {
      [[randomhue]]
    }
  },

  ui = {
    border = [[double]]
  },

  change_detection = {
    enabled = false
  }
})
