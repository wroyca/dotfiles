--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

local shada = vim.o.shada
vim.o.shada = ""
vim.schedule(function()
  vim.o.shada = shada
  pcall(vim.cmd.rshada, { bang = true })
  vim.cmd.doautocmd "User ShadaLoadPost"
end)

vim.g.mapleader      = " "
vim.g.localmapleader = ","
vim.o.mouse          = "a"
vim.o.mousescroll    = "ver:3,hor:0"
vim.o.mousemoveevent = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.number         = true
vim.o.cursorline     = true
vim.o.more           = false
vim.o.wrap           = false
vim.o.laststatus     = 3
vim.o.scrolloff      = 4
vim.o.pumheight      = 8
vim.o.fillchars      = "eob: "
vim.o.guicursor      = "a:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20"

-- Most formatting and style settings will be automatically managed by
-- EditorConfig (https://editorconfig.org/), provided it is available and in
-- use.
--
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.expandtab      = true
vim.o.preserveindent = true
vim.o.smartindent    = true
vim.o.tabstop        = 2
vim.o.shiftwidth     = 0
vim.o.shiftround     = true

-- Nvim bundles a clipboard provider that allows copying to the system
-- clipboard using OSC 52. OSC 52 is an Operating System Command control
-- sequence that writes the copied text to the terminal emulator. If the
-- terminal emulator supports OSC 52 then it will write the copied text into
-- the system clipboard. The rational here is to avoid specific bugs and
-- limitations related to wayland clipboard functionality, more specifically:
--
-- https://github.com/bugaevc/wl-clipboard/issues/12
-- https://github.com/bugaevc/wl-clipboard/issues/210
-- https://gitlab.gnome.org/GNOME/mutter/-/issues/524
-- https://github.com/bugaevc/wl-clipboard/issues/206#issuecomment-1858913481
-- https://github.com/neovim/neovim/issues/24470
--
vim.o.clipboard = "unnamedplus"
vim.g.clipboard = {
  name = "OSC 52",
  copy = {
    ["+"] = require("vim.ui.clipboard.osc52").copy("+"),
    ["*"] = require("vim.ui.clipboard.osc52").copy("*"),
  },
  paste = {
    ["+"] = require("vim.ui.clipboard.osc52").paste("+"),
    ["*"] = require("vim.ui.clipboard.osc52").paste("*"),
  },
}
