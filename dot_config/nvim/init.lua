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
vim.cmd.hi           [[NormalFloat cterm=reverse guibg=NONE]]

local lazypath = vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string]], [[lazy]], [[lazy.nvim]])
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

require [[lazy]].setup({
  spec = {
    { name = [[coc]],                              [[neoclide/coc.nvim]],                                                  branch=[[release]]                                    },
    { name = [[neodev]],                           [[folke/neodev.nvim]],                                                                                                        },

    { name = [[mini.ai]],                          [[mini.ai]],                                                                                                       dev = true },
    { name = [[mini.align]],                       [[mini.align]],                                                                                                    dev = true },
    { name = [[mini.animate]],                     [[mini.animate]],                                                                                                  dev = true },
    { name = [[mini.base16]],                      [[mini.base16]],                                                                                                   dev = true },
    { name = [[mini.basics]],                      [[mini.basics]],                                                                                                   dev = true },
    { name = [[mini.bracketed]],                   [[mini.bracketed]],                                                                                                dev = true },
    { name = [[mini.bufremove]],                   [[mini.bufremove]],                                                                                                dev = true },
    { name = [[mini.clue]],                        [[mini.clue]],                                                                                                     dev = true },
    { name = [[mini.colors]],                      [[mini.colors]],                                                                                                   dev = true },
    { name = [[mini.comment]],                     [[mini.comment]],                                                                                                  dev = true },
    { name = [[mini.completion]],                  [[mini.completion]],                                                                                               dev = true },
    { name = [[mini.cursorword]],                  [[mini.cursorword]],                                                                                               dev = true },
    { name = [[mini.deps]],                        [[mini.deps]],                                                                                                     dev = true },
    { name = [[mini.diff]],                        [[mini.diff]],                                                                                                     dev = true },
    { name = [[mini.doc]],                         [[mini.doc]],                                                                                                      dev = true },
    { name = [[mini.extra]],                       [[mini.extra]],                                                                                                    dev = true },
    { name = [[mini.files]],                       [[mini.files]],                                                                                                    dev = true },
    { name = [[mini.fuzzy]],                       [[mini.fuzzy]],                                                                                                    dev = true },
    { name = [[mini.hipatterns]],                  [[mini.hipatterns]],                                                                                               dev = true },
    { name = [[mini.hues]],                        [[mini.hues]],                                                                                                     dev = true },
    { name = [[mini.icons]],                       [[mini.icons]],                                                                                                    dev = true },
    { name = [[mini.indentscope]],                 [[mini.indentscope]],                                                                                              dev = true },
    { name = [[mini.jump2d]],                      [[mini.jump2d]],                                                                                                   dev = true },
    { name = [[mini.jump]],                        [[mini.jump]],                                                                                                     dev = true },
    { name = [[mini.map]],                         [[mini.map]],                                                                                                      dev = true },
    { name = [[mini.misc]],                        [[mini.misc]],                                                                                                     dev = true },
    { name = [[mini.move]],                        [[mini.move]],                                                                                                     dev = true },
    { name = [[mini.notify]],                      [[mini.notify]],                                                                                                   dev = true },
    { name = [[mini.operators]],                   [[mini.operators]],                                                                                                dev = true },
    { name = [[mini.pairs]],                       [[mini.pairs]],                                                                                                    dev = true },
    { name = [[mini.pick]],                        [[mini.pick]],                                                                                                     dev = true },
    { name = [[mini.sessions]],                    [[mini.sessions]],                                                                                                 dev = true },
    { name = [[mini.splitjoin]],                   [[mini.splitjoin]],                                                                                                dev = true },
    { name = [[mini.starter]],                     [[mini.starter]],                                                                                                  dev = true },
    { name = [[mini.statusline]],                  [[mini.statusline]],                                                                                               dev = true },
    { name = [[mini.surround]],                    [[mini.surround]],                                                                                                 dev = true },
    { name = [[mini.tabline]],                     [[mini.tabline]],                                                                                                  dev = true },
    { name = [[mini.test]],                        [[mini.test]],                                                                                                     dev = true },
    { name = [[mini.trailspace]],                  [[mini.trailspace]],                                                                                               dev = true },
    { name = [[mini.visits]],                      [[mini.visits]],                                                                                                   dev = true },
    { name = [[mini]],                             [[echasnovski/mini.nvim]],                                                                                       lazy = false },

    { name = [[bqf]],                              [[kevinhwang91/nvim-bqf]]                                                                                                     },
    { name = [[fundo]],                            [[kevinhwang91/nvim-fundo]]                                                                                                   },
    { name = [[hlslens]],                          [[kevinhwang91/nvim-hlslens]]                                                                                                 },
    { name = [[ufo]],                              [[kevinhwang91/nvim-ufo]]                                                                                                     },

    { name = [[neogit]],                           [[NeogitOrg/neogit]]                                                                                                          },
    { name = [[neogit-telescope]],                 [[nvim-telescope/telescope.nvim]]                                                                                             },
    { name = [[neogit-diffview]],                  [[sindrets/diffview.nvim]]                                                                                                    },

    { name = [[leap.flit]],                        [[ggandor/flit.nvim]]                                                                                                         },
    { name = [[leap.spooky]],                      [[ggandor/leap-spooky.nvim]]                                                                                                  },
    { name = [[leap]],                             [[ggandor/leap.nvim]],                                                  commit = [[90ca1ded0608c891ba7a63de059ce5bc8533f060]] },

    { name = [[treesitter-context]],               [[nvim-treesitter/nvim-treesitter-context]]                                                                                   },
    { name = [[treesitter-textobjects]],           [[nvim-treesitter/nvim-treesitter-textobjects]]                                                                               },
    { name = [[treesitter]],                       [[nvim-treesitter/nvim-treesitter]]                                                                                           },

    { name = [[nui]],                              [[MunifTanjim/nui.nvim]]                                                                                                      },
    { name = [[nui-components]],                   [[grapp-dev/nui-components.nvim]]                                                                                             },
    { name = [[plenary]],                          [[nvim-lua/plenary.nvim]]                                                                                                     },
    { name = [[promise-async]],                    [[kevinhwang91/promise-async]]                                                                                                },

    -- Merge specs together in the final spec.
    {
      import = [[spec]]
    }
  },

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

  rocks = {
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
    backdrop = 100
  }
})
