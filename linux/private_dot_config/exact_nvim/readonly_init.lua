--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

local shada = vim.o.shada
vim.o.shada = [[]]
vim.schedule(function()
  vim.o.shada = shada
  pcall(vim.cmd.rshada, { bang = true })
end)

vim.g.mapleader      = [[ ]]
vim.g.localmapleader = [[,]]
vim.o.clipboard      = [[unnamedplus]]
vim.o.fileencoding   = [[utf-8]]
vim.o.signcolumn     = [[yes:1]]
vim.o.splitkeep      = [[screen]]
vim.o.titlestring    = [[%t]]
vim.o.virtualedit    = [[onemore]]
vim.o.whichwrap      = 'b,s,h,l,<,>,~,[,]'
vim.o.number         = false
vim.wo.wrap          = true
vim.o.list           = true
vim.o.title          = true
vim.o.confirm        = true
vim.o.splitbelow     = true
vim.o.splitright     = true
vim.o.termguicolors  = true
vim.o.undofile       = true
vim.o.expandtab      = true
vim.o.breakindent    = true
vim.o.smartindent    = true
vim.o.preserveindent = true
vim.o.cursorline     = true
vim.o.gdefault       = true
vim.o.tabstop        = 2
vim.o.shiftwidth     = 0
vim.o.scrolloff      = 4
vim.o.pumheight      = 8
vim.o.cmdheight      = 0
vim.o.laststatus     = 0
vim.o.updatetime     = 300
vim.o.foldlevel      = 99
vim.o.foldlevelstart = 99
vim.opt.fillchars    = {eob = [[ ]]}
vim.opt.cinkeys      : remove [[:]]
vim.opt.indentkeys   : remove [[:]]
vim.opt.suffixes     : remove [[.h]]
vim.opt.guicursor    : append [[n-v-c:block]]
vim.opt.guicursor    : append [[i-ci-ve:ver25]]
vim.opt.guicursor    : append [[r-cr:hor20]]
vim.opt.guicursor    : append [[o:hor50]]
vim.opt.guicursor    : append [[a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor]]
vim.opt.guicursor    : append [[sm:block-blinkwait175-blinkoff150-blinkon175]]

vim.keymap.set({ [[n]], [[v]]        }, [[<Space>]], [[<Nop>]], { silent = true })
vim.keymap.set({ [[n]], [[x]], [[i]] }, [[<F1>]],    [[<Nop>]], { silent = true })

local lazypath = vim.fn.stdpath [[data]] .. [[/lazy/lazy.nvim]]
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

local cache = nil

vim.fn.jobstart({ [[kitty]], [[@]], [[get-colors]] }, {
  on_stdout = function(_, d, _)
    for _, result in ipairs(d) do
      if string.match(result, [[^background]]) then
        cache = vim.split(result, [[%s+]])[2]
        break
      end
    end
  end,
  on_stderr = function(_, d, _)
    if #d > 1 then
      vim.api.nvim_err_writeln(
        [[Error getting background. Make sure kitty remote control is turned on.]]
      )
    end
  end
})

local set = function(color, sync)
  local command = [[kitty @ set-colors background=]] .. color
  if not sync then
    vim.fn.jobstart(command, {
      on_stderr = function(_, d, _)
        if #d > 1 then
          vim.api.nvim_err_writeln(
            [[Error changing background. Make sure kitty remote control is turned on.]]
          )
        end
      end
    })
  else
    vim.fn.system(command)
  end
end

vim.api.nvim_create_autocmd({ [[VimEnter]], [[VimResume]] }, {
  pattern = "*",
  callback = function()
    set(string.format([[#%06X]], vim.api.nvim_get_hl(0, { name = [[Normal]] }).bg))
  end
})

vim.api.nvim_create_autocmd([[User]], {
  pattern = { [[LumenLight]], [[LumenDark]] },
  callback = function()
    set(string.format([[#%06X]], vim.api.nvim_get_hl(0, { name = [[Normal]] }).bg))
  end
})

vim.api.nvim_create_autocmd({ [[VimLeavePre]], [[VimSuspend]] }, {
  callback = function()
    set(cache, true)
  end
})

require [[lazy]].setup({
  spec = {
    { name = [[coc]],                              [[neoclide/coc.nvim]],                                                                                                        },
    { name = [[neodev]],                           [[folke/neodev.nvim]],                                                                                                        },

    { name = [[mini.ai]],                          [[mini.ai]],                                                            dev = true                                            },
    { name = [[mini.align]],                       [[mini.align]],                                                         dev = true                                            },
    { name = [[mini.animate]],                     [[mini.animate]],                                                       dev = true                                            },
    { name = [[mini.base16]],                      [[mini.base16]],                                                        dev = true                                            },
    { name = [[mini.basics]],                      [[mini.basics]],                                                        dev = true                                            },
    { name = [[mini.bracketed]],                   [[mini.bracketed]],                                                     dev = true                                            },
    { name = [[mini.bufremove]],                   [[mini.bufremove]],                                                     dev = true                                            },
    { name = [[mini.clue]],                        [[mini.clue]],                                                          dev = true                                            },
    { name = [[mini.colors]],                      [[mini.colors]],                                                        dev = true                                            },
    { name = [[mini.comment]],                     [[mini.comment]],                                                       dev = true                                            },
    { name = [[mini.completion]],                  [[mini.completion]],                                                    dev = true                                            },
    { name = [[mini.cursorword]],                  [[mini.cursorword]],                                                    dev = true                                            },
    { name = [[mini.doc]],                         [[mini.doc]],                                                           dev = true                                            },
    { name = [[mini.extra]],                       [[mini.extra]],                                                         dev = true                                            },
    { name = [[mini.files]],                       [[mini.files]],                                                         dev = true                                            },
    { name = [[mini.fuzzy]],                       [[mini.fuzzy]],                                                         dev = true                                            },
    { name = [[mini.hipatterns]],                  [[mini.hipatterns]],                                                    dev = true                                            },
    { name = [[mini.hues]],                        [[mini.hues]],                                                          dev = true                                            },
    { name = [[mini.indentscope]],                 [[mini.indentscope]],                                                   dev = true                                            },
    { name = [[mini.jump2d]],                      [[mini.jump2d]],                                                        dev = true                                            },
    { name = [[mini.jump]],                        [[mini.jump]],                                                          dev = true                                            },
    { name = [[mini.map]],                         [[mini.map]],                                                           dev = true                                            },
    { name = [[mini.misc]],                        [[mini.misc]],                                                          dev = true                                            },
    { name = [[mini.move]],                        [[mini.move]],                                                          dev = true                                            },
    { name = [[mini.notify]],                      [[mini.notify]],                                                        dev = true                                            },
    { name = [[mini.operators]],                   [[mini.operators]],                                                     dev = true                                            },
    { name = [[mini.pairs]],                       [[mini.pairs]],                                                         dev = true                                            },
    { name = [[mini.pick]],                        [[mini.pick]],                                                          dev = true                                            },
    { name = [[mini.sessions]],                    [[mini.sessions]],                                                      dev = true                                            },
    { name = [[mini.splitjoin]],                   [[mini.splitjoin]],                                                     dev = true                                            },
    { name = [[mini.starter]],                     [[mini.starter]],                                                       dev = true                                            },
    { name = [[mini.statusline]],                  [[mini.statusline]],                                                    dev = true                                            },
    { name = [[mini.surround]],                    [[mini.surround]],                                                      dev = true                                            },
    { name = [[mini.tabline]],                     [[mini.tabline]],                                                       dev = true                                            },
    { name = [[mini.test]],                        [[mini.test]],                                                          dev = true                                            },
    { name = [[mini.trailspace]],                  [[mini.trailspace]],                                                    dev = true                                            },
    { name = [[mini.visits]],                      [[mini.visits]],                                                        dev = true                                            },
    { name = [[mini]],                             [[echasnovski/mini.nvim]]                                                                                                     },

    { name = [[misc-bqf]],                         [[kevinhwang91/nvim-bqf]]                                                                                                     },
    { name = [[misc-fundo]],                       [[kevinhwang91/nvim-fundo]]                                                                                                   },
    { name = [[misc-gitsigns]],                    [[lewis6991/gitsigns.nvim]]                                                                                                   },
    { name = [[misc-hlslens]],                     [[kevinhwang91/nvim-hlslens]]                                                                                                 },
    { name = [[misc-last-color]],                  [[raddari/last-color.nvim]]                                                                                                   },
    { name = [[misc-leap-flit]],                   [[ggandor/flit.nvim]]                                                                                                         },
    { name = [[misc-leap-spooky]],                 [[ggandor/spooky.nvim]]                                                                                                       },
    { name = [[misc-leap]],                        [[ggandor/leap.nvim]],                                                  commit = [[90ca1ded0608c891ba7a63de059ce5bc8533f060]] },
    { name = [[misc-lumen]],                       [[vimpostor/vim-lumen]]                                                                                                       },
    { name = [[misc-neogit]],                      [[neogitorg/neogit]],                                                                                                         },
    { name = [[misc-range-highlight-cmd-parser]],  [[winston0410/cmd-parser.nvim]]                                                                                               },
    { name = [[misc-range-highlight]],             [[winston0410/range-highlight.nvim]]                                                                                          },
    { name = [[misc-statuscol]],                   [[luukvbaal/statuscol.nvim]]                                                                                                  },
    { name = [[misc-ufo]],                         [[kevinhwang91/nvim-ufo]]                                                                                                     },
    { name = [[misc-visual-multi]],                [[mg979/vim-visual-multi]]                                                                                                    },
    { name = [[misc-focus]],                       [[nvim-focus/focus.nvim]]                                                                                                     },

    { name = [[telescope]],                        [[nvim-telescope/telescope.nvim]]                                                                                             },

    { name = [[treesitter-context]],               [[nvim-treesitter/nvim-treesitter-context]]                                                                                   },
    { name = [[treesitter-docs]],                  [[nvim-treesitter/nvim-tree-docs]]                                                                                            },
    { name = [[treesitter-refactor]],              [[nvim-treesitter/nvim-treesitter-refactor]]                                                                                  },
    { name = [[treesitter-textobjects]],           [[nvim-treesitter/nvim-treesitter-textobjects]]                                                                               },
    { name = [[treesitter]],                       [[nvim-treesitter/nvim-treesitter]]                                                                                           },

    { name = [[nui]],                              [[MunifTanjim/nui.nvim]]                                                                                                      },
    { name = [[plenary]],                          [[nvim-lua/plenary.nvim]]                                                                                                     },
    { name = [[promise-async]],                    [[kevinhwang91/promise-async]]                                                                                                },

    {
      import = [[spec]]
    }
  },

  performance = {
    rtp = {
      disabled_plugins = {
        [[2html_plugin]],
        [[bugreport]],
        [[compiler]],
        [[ftplugin]],
        [[getscriptPlugin]],
        [[getscript]],
        [[gzip]],
        [[health]],
        [[logipat]],
        [[matchit]],
        [[netrwFileHandlers]],
        [[netrwPlugin]],
        [[netrwSettings]],
        [[netrw]],
        [[nvim]],
        [[optwin]],
        [[rplugin]],
        [[rrhelper]],
        [[shada]],
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

  change_detection = {
    enabled = false
  },

  install = {
    colorscheme = {
      [[default]]
    }
  },

  ui = {
    pills = false
  }
})

if vim.o.filetype == [[lazy]] then vim.cmd.close() end
