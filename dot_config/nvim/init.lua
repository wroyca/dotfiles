--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd ("vim-lumen")

local shada = vim.o.shada
vim.o.shada = ""
vim.schedule (function ()
  vim.o.shada = shada
  pcall (vim.cmd.rshada, { bang = true })
  vim.cmd.doautocmd ("User ShadaLoadPost")
end)

vim.g.mapleader      = vim.keycode ("<Space>")
vim.g.localmapleader = vim.g.mapleader
vim.o.clipboard      = "unnamedplus"
vim.o.guicursor      = "a:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20"
vim.o.mouse          = "a"
vim.o.mousescroll    = "ver:3,hor:0"
vim.o.mousemoveevent = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.number         = true
vim.o.gdefault       = true
vim.o.undofile       = true
vim.o.cursorline     = true
vim.o.termguicolors  = true
vim.o.scrolloff      = 4
vim.o.pumheight      = 8
vim.o.laststatus     = 3
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.smartindent    = true
vim.o.preserveindent = true
