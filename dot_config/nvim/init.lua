--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd [[vim-lumen]]

local shada = vim.o.shada
vim.o.shada = [[]]
vim.schedule(function()
  vim.o.shada = shada
  pcall(vim.cmd.rshada, { bang = true })
  vim.cmd.doautocmd [[User ShadaLoadPost]]
end)

vim.g.mapleader      = [[ ]]
vim.g.localmapleader = [[,]]
vim.o.clipboard      = [[unnamedplus]]
vim.o.mouse          = [[a]]
vim.o.mousescroll    = [[ver:3,hor:0]]
vim.o.mousemoveevent = true
vim.o.fillchars      = [[eob: ]]
vim.o.titlestring    = [[%t]]
vim.o.title          = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.termguicolors  = true
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.expandtab      = true
vim.o.preserveindent = true
vim.o.smartindent    = true
vim.o.shiftwidth     = 0
vim.o.tabstop        = 2
vim.o.textwidth      = 80
vim.o.cmdheight      = 1
vim.o.laststatus     = 3
vim.o.pumheight      = 8
vim.o.scrolloff      = 4
vim.o.wrap           = false
vim.o.more           = false
vim.o.number         = true

local lazypath = vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string ]], [[lazy]], [[lazy.nvim]])
if not vim.uv.fs_stat(lazypath) then
  vim.system({
    [[git]],
    [[clone]],
    [[--filter=blob:none]],
    [[https://github.com/folke/lazy.nvim]],
    lazypath
  }):wait()
end
vim.opt.rtp:prepend(lazypath)

require [[lazy]].setup([[spec]], {
  performance = {
    rtp = {
      disabled_plugins = {
        [[2html_plugin]],
        [[bugreport]],
        [[ftplugin]],
        [[getscriptPlugin]],
        [[getscript]],
        [[gzip]],
        [[health]],
        [[logipat]],
        [[matchit]],
        [[matchparen]],
        [[netrwFileHandlers]],
        [[netrwPlugin]],
        [[netrwSettings]],
        [[netrw]],
        [[nvim]],
        [[optwin]],
        [[rplugin]],
        [[rrhelper]],
        [[spellfile]],
        [[spellfile_plugin]],
        [[synmenu]],
        [[syntax]],
        [[tarPlugin]],
        [[tar]],
        [[tohtml]],
        [[tutor]],
        [[vimballPlugin]],
        [[vimball]],
        [[zipPlugin]],
        [[zip]]
      }
    }
  },

  defaults = {
    lazy = true,
    version = false
  },

  pkg = {
    enabled = false
  },

  git = {
    timeout = 60
  },

  readme = {
    enabled = false
  },

  change_detection = {
    enabled = false
  },

  install = {
    colorscheme = {
      [[default]]
    }
  },

  ui = {
    pills = false,
    border = [[single]],
    backdrop = 100
  }
})
