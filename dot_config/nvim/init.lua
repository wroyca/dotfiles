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
end)

vim.g.mapleader      = vim.keycode("<Space>")
vim.g.localmapleader = vim.g.mapleader
vim.o.clipboard      = "unnamedplus"
vim.o.guicursor      = "a:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20"
vim.o.mouse          = "a"
vim.o.mousescroll    = "ver:3,hor:0"
vim.o.mousemoveevent = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.cursorline     = true
vim.o.scrolloff      = 4
vim.o.pumheight      = 8
vim.o.laststatus     = 0
vim.o.cmdheight      = 0
vim.o.wrap           = false
vim.o.fillchars      = "eob: "
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.expandtab      = true
vim.o.preserveindent = true
vim.o.smartindent    = true
vim.o.tabstop        = 2
vim.o.shiftwidth     = 0
vim.o.shiftround     = true

local autocmds = {
  {
    event = "FileType",
    desc = "Load EditorConfig settings with highest priority",
    callback = function(ev)
      if vim.F.if_nil(vim.b.editorconfig, vim.g.editorconfig, true) then
        local editorconfig_avail, editorconfig = pcall(require, "editorconfig")
        if editorconfig_avail then
          editorconfig.config(ev.buf)
        end
      end
    end
  },
  {
    event = "BufWritePre",
    desc = "Create any missing parent directories before saving the file",
    callback = function(ev)
      local buf_is_valid_and_listed = vim.api.nvim_buf_is_valid(ev.buf) and vim.bo[ev.buf].buflisted
      if buf_is_valid_and_listed then
        vim.fn.mkdir(vim.fn.fnamemodify(vim.uv.fs_realpath(ev.match) or ev.match, ":p:h"), "p")
      end
    end
  },
  {
    event = { "InsertLeave", "WinEnter" },
    desc = "Enable cursor line in the active window",
    callback = function(event)
      if vim.bo[event.buf].buftype == "" then
        vim.opt_local.cursorline = true
      end
    end
  },
  {
    event = { "InsertEnter", "WinLeave" },
    desc = "Disable cursor line in inactive windows",
    callback = function()
      vim.opt_local.cursorline = false
    end
  },
  {
    event = "VimResized",
    desc = "Adjust split sizes when the window is resized",
    callback = function()
      local current_tab = vim.api.nvim_get_current_tabpage()
      vim.cmd.tabdo("wincmd =")
      vim.api.nvim_set_current_tabpage(current_tab)
    end
  }
}

for _, autocmd in ipairs(autocmds) do
  vim.api.nvim_create_autocmd(autocmd.event, { desc = autocmd.desc, callback = autocmd.callback })
end
