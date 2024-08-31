--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd "vim-lumen"

local shada = vim.o.shada
vim.o.shada = ""
vim.schedule(function()
  vim.o.shada = shada
  pcall(vim.cmd.rshada, { bang = true })
  vim.cmd.doautocmd "User ShadaLoadPost"
end)

vim.g.mapleader      = " "
vim.g.localmapleader = ","
vim.o.clipboard      = "unnamedplus"
vim.o.mouse          = "a"
vim.o.mousescroll    = "ver:3,hor:0"
vim.o.mousemoveevent = true
vim.o.title          = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.termguicolors  = true
vim.o.ruler          = false
vim.o.wrap           = false
vim.o.showmode       = false
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.expandtab      = true
vim.o.preserveindent = true
vim.o.smartindent    = true
vim.o.shiftwidth     = 0
vim.o.tabstop        = 2
vim.o.laststatus     = 3
vim.o.showtabline    = 0
vim.o.cmdheight      = 1
vim.o.pumheight      = 8
vim.o.scrolloff      = 4
vim.o.fillchars      = "eob: "
vim.o.guicursor      = "n-v-i:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20"
vim.opt.diffopt      = vim.list_extend(vim.opt.diffopt:get(), { "algorithm:histogram" })

--------------------------------------------------
-- Colemak
--------------------------------------------------
local map = vim.keymap.set

local function swap_map(lhs, rhs, mode)
  map(mode or "", lhs, rhs, {})
  map(mode or "", rhs, lhs, {})
end

local colemak_maps = {
  { "n", "j" }, -- down
  { "e", "k" }, -- up
  { "s", "h" }, -- left
  { "t", "l" }, -- right
}
local mvmnt_prefix = "<C-w><C-"

for _, pairs in ipairs(colemak_maps) do
  local lhs = pairs[1]
  local rhs = pairs[2]

  local mvmnt_lhs = table.concat({ mvmnt_prefix, lhs, ">" })
  local mvmnt_rhs = table.concat({ mvmnt_prefix, rhs, ">" })

  -- lowercase
  swap_map(lhs, rhs)

  -- uppercase
  swap_map(string.upper(lhs), string.upper(rhs))

  -- window movement
  swap_map(mvmnt_lhs, mvmnt_rhs)
end

for _, mode in pairs({ "n", "v" }) do
  map(mode, "e", "v:count == 0 ? 'gk' : 'k'", expr)
  map(mode, "n", "v:count == 0 ? 'gj' : 'j'", expr)
end 
