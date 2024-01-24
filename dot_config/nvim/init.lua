--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

local h = io.popen([[
gdbus call --session \
           --dest=org.freedesktop.portal.Desktop \
           --object-path=/org/freedesktop/portal/desktop \
           --method=org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme
]])

if h ~= nil then
  if string.match(h:read('*a'), [[ %d]]) == [[ 1]] then
    h:close()
    vim.o.background = [[dark]]
  else
    h:close()
    vim.o.background = [[light]]
  end
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
vim.o.winblend       = 0
vim.o.tabstop        = 2
vim.o.shiftwidth     = 0
vim.o.scrolloff      = 5
vim.o.pumheight      = 9
vim.o.cmdheight      = 0
vim.o.laststatus     = 0
vim.o.foldlevel      = 99
vim.o.foldlevelstart = 99
vim.o.foldenable     = true
vim.o.list           = true
vim.opt.fillchars    : append { diff = [[╱]] }
vim.opt.suffixes     : remove [[.h]]
vim.opt.cinkeys      : remove [[:]]
vim.opt.indentkeys   : remove [[:]]
vim.opt.shortmess    : append [[sI]]
vim.opt.guicursor    : append [[n-v-c-sm:block-Cursor]]
vim.opt.guicursor    : append [[i-ci-ve:ver25-Cursor]]
vim.opt.guicursor    : append [[r-cr-o:hor20-Cursor]]
vim.diagnostic       . config ({ signs = false, virtual_text = false })

local lazypath = vim.fn.stdpath [[data]] .. [[/lazy/lazy.nvim]]
if not vim.uv.fs_stat(lazypath) then
  vim.system({
    [[git]],
    [[clone]],
    [[--filter=blob:none]],
    [[https://github.com/folke/lazy.nvim]],
    lazypath
  }, { text = true }):wait()
end
vim.opt.rtp:prepend(lazypath)

require [[keys]]
require [[cmds]]
require [[lazy]].setup({
  spec = {
    { import = [[spec]] },

    { name = [[cmp]],                          [[hrsh7th/nvim-cmp]] },
    { name = [[cmp-buffer]],                   [[hrsh7th/cmp-buffer]] },
    { name = [[cmp-calc]],                     [[hrsh7th/cmp-calc]] },
    { name = [[cmp-cmdline]],                  [[hrsh7th/cmp-cmdline]] },
    { name = [[cmp-copilot]],                  [[hrsh7th/cmp-copilot]] },
    { name = [[cmp-emoji]],                    [[hrsh7th/cmp-emoji]] },
    { name = [[cmp-git]],                      [[hrsh7th/cmp-git]] },
    { name = [[cmp-latex-symbols]],            [[hrsh7th/cmp-latex-symbols]] },
    { name = [[cmp-look]],                     [[hrsh7th/cmp-look]] },
    { name = [[cmp-luasnip]],                  [[saadparwaiz1/cmp_luasnip]] },
    { name = [[cmp-luasnip-loaders]],          [[L3MON4D3/LuaSnip]] },
    { name = [[cmp-nvim-lsp]],                 [[hrsh7th/cmp-nvim-lsp]] },
    { name = [[cmp-nvim-lsp-document-symbol]], [[hrsh7th/cmp-nvim-lsp-document-symbol]] },
    { name = [[cmp-nvim-lsp-signature-help]],  [[hrsh7th/cmp-nvim-lsp-signature-help]] },
    { name = [[cmp-nvim-lua]],                 [[hrsh7th/cmp-nvim-lua]] },
    { name = [[cmp-omni]],                     [[hrsh7th/cmp-omni]] },
    { name = [[cmp-path]],                     [[hrsh7th/cmp-path]] },
    { name = [[cmp-vsnip]],                    [[hrsh7th/cmp-vsnip]] },
    { name = [[cmp-vsnip-loaders]],            [[hrsh7th/vim-vsnip]] },

    { name = [[lspconfig]],                    [[neovim/nvim-lspconfig]] },
    { name = [[lspconfig-refactoring]],        [[ThePrimeagen/refactoring.nvim]] },
    { name = [[lspconfig-rulebook]],           [[chrisgrieser/nvim-rulebook]] },
    { name = [[lspconfig-output-panel]],       [[mhanberg/output-panel.nvim]] },

    { name = [[mason]],                        [[williamboman/mason.nvim]] },
    { name = [[mason-dap]],                    [[jay-babu/mason-nvim-dap.nvim]] },
    { name = [[mason-lspconfig]],              [[williamboman/mason-lspconfig.nvim]] },

    { name = [[mini]],                         [[echasnovski/mini.nvim]] },
    { name = [[mini.ai]],                      [[mini.ai]],                  dev = true },
    { name = [[mini.align]],                   [[mini.align]],               dev = true },
    { name = [[mini.animate]],                 [[mini.animate]],             dev = true },
    { name = [[mini.base16]],                  [[mini.base16]],              dev = true },
    { name = [[mini.basics]],                  [[mini.basics]],              dev = true },
    { name = [[mini.bracketed]],               [[mini.bracketed]],           dev = true },
    { name = [[mini.bufremove]],               [[mini.bufremove]],           dev = true },
    { name = [[mini.clue]],                    [[mini.clue]],                dev = true },
    { name = [[mini.colors]],                  [[mini.colors]],              dev = true },
    { name = [[mini.comment]],                 [[mini.comment]],             dev = true },
    { name = [[mini.completion]],              [[mini.completion]],          dev = true },
    { name = [[mini.cursorword]],              [[mini.cursorword]],          dev = true },
    { name = [[mini.doc]],                     [[mini.doc]],                 dev = true },
    { name = [[mini.extra]],                   [[mini.extra]],               dev = true },
    { name = [[mini.files]],                   [[mini.files]],               dev = true },
    { name = [[mini.fuzzy]],                   [[mini.fuzzy]],               dev = true },
    { name = [[mini.hipatterns]],              [[mini.hipatterns]],          dev = true },
    { name = [[mini.hues]],                    [[mini.hues]],                dev = true },
    { name = [[mini.indentscope]],             [[mini.indentscope]],         dev = true },
    { name = [[mini.jump]],                    [[mini.jump]],                dev = true },
    { name = [[mini.jump2d]],                  [[mini.jump2d]],              dev = true },
    { name = [[mini.map]],                     [[mini.map]],                 dev = true },
    { name = [[mini.misc]],                    [[mini.misc]],                dev = true },
    { name = [[mini.move]],                    [[mini.move]],                dev = true },
    { name = [[mini.notify]],                  [[mini.notify]],              dev = true },
    { name = [[mini.operators]],               [[mini.operators]],           dev = true },
    { name = [[mini.pairs]],                   [[mini.pairs]],               dev = true },
    { name = [[mini.pick]],                    [[mini.pick]],                dev = true },
    { name = [[mini.sessions]],                [[mini.sessions]],            dev = true },
    { name = [[mini.splitjoin]],               [[mini.splitjoin]],           dev = true },
    { name = [[mini.starter]],                 [[mini.starter]],             dev = true },
    { name = [[mini.statusline]],              [[mini.statusline]],          dev = true },
    { name = [[mini.surround]],                [[mini.surround]],            dev = true },
    { name = [[mini.tabline]],                 [[mini.tabline]],             dev = true },
    { name = [[mini.test]],                    [[mini.test]],                dev = true },
    { name = [[mini.trailspace]],              [[mini.trailspace]],          dev = true },
    { name = [[mini.visits]],                  [[mini.visits]],              dev = true },

    { name = [[misc-fundo]],                   [[kevinhwang91/nvim-fundo]] },
    { name = [[misc-ufo]],                     [[kevinhwang91/nvim-ufo]] },
    { name = [[misc-bqf]],                     [[kevinhwang91/nvim-bqf]] },
    { name = [[misc-hlslens]],                 [[kevinhwang91/nvim-hlslens]] },
    { name = [[misc-leap]],                    [[ggandor/leap.nvim]] },
    { name = [[misc-leap-flit]],               [[ggandor/flit.nvim]] },
    { name = [[misc-leap-spooky]],             [[ggandor/leap-spooky.nvim]] },
    { name = [[misc-cmd-parser]],              [[winston0410/cmd-parser.nvim]] },
    { name = [[misc-range-highlight]],         [[winston0410/range-highlight.nvim]] },
    { name = [[misc-visual-multi]],            [[mg979/vim-visual-multi]] },
    { name = [[misc-neogit]],                  [[NeogitOrg/neogit]], branch = [[nightly]] },
    { name = [[misc-gitsigns]],                [[lewis6991/gitsigns.nvim]] },
    { name = [[misc-chameleon]],               [[shaun-mathew/chameleon.nvim]] },
    { name = [[misc-auto-dark-mode]],          [[f-person/auto-dark-mode.nvim]] },
    { name = [[misc-last-color]],              [[raddari/last-color.nvim]] },

    { name = [[treesitter]],                   [[nvim-treesitter/nvim-treesitter]] },
    { name = [[treesitter-context]],           [[nvim-treesitter/nvim-treesitter-context]] },
    { name = [[treesitter-docs]],              [[nvim-treesitter/nvim-tree-docs]] },
    { name = [[treesitter-refactor]],          [[nvim-treesitter/nvim-treesitter-refactor]] },
    { name = [[treesitter-textobjects]],       [[nvim-treesitter/nvim-treesitter-textobjects]] },

    { name = [[nui]],                          [[MunifTanjim/nui.nvim]] },
    { name = [[plenary]],                      [[nvim-lua/plenary.nvim]] },
    { name = [[promise-async]],                [[kevinhwang91/promise-async]] }
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
        [[man]],
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
      [[randomhue]],
      [[default]]
    }
  },

  ui = {
    title = [[Lazy]],
    border = [[single]]
  }
})
