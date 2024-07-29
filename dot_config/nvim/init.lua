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
vim.o.title          = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.termguicolors  = true
vim.o.number         = true
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.expandtab      = true
vim.o.preserveindent = true
vim.o.smartindent    = true
vim.o.shiftwidth     = 0
vim.o.tabstop        = 2
vim.o.cmdheight      = 1
vim.o.laststatus     = 3
vim.o.pumheight      = 8
vim.o.scrolloff      = 4
vim.o.guicursor      = [[n-v-i:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20]]

vim.api.nvim_create_autocmd({
  [[VimEnter]],
  [[VimResume]],
  [[ColorScheme]]
}, {
  callback = function()
    io.stdout:write(string.format("\027]10;1;#%06x\a", vim.api.nvim_get_hl(0, { name = [[Normal]] }).bg))
  end
})

vim.api.nvim_create_autocmd({ [[User]] }, {
  pattern = {
    [[LumenDark]],
    [[LumenLight]]
  },
  callback = function()
    io.stdout:write(string.format("\027]10;1;#%06x\a", vim.api.nvim_get_hl(0, { name = [[Normal]] }).bg))
  end,
})

vim.api.nvim_create_autocmd({
  [[VimLeavePre]],
  [[VimSuspend]]
}, {
  callback = function()
    io.stdout:write("\027]111;;\a")
  end
})
