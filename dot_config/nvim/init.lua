--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd "lumen"

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
vim.o.laststatus     = 3
vim.o.wrap           = false
vim.o.fillchars      = "eob: "

local function set_editorconfig_properties (props)
  local ok, p = pcall(function() return require("editorconfig").properties end)
  if not ok then
    vim.notify("warning: editorconfig runtime is unavailable", vim.log.levels.WARN)
    return
  end
  for _, n in ipairs(props) do
    p[n] = function(b, v, _)
      vim.b[b][n] = v
    end
  end
end

set_editorconfig_properties({
  "breakindent",
  "copyindent",
  "smartindent",
  "preserveindent",
})

local function create_autocmds(cmds)
  for _, c in ipairs(cmds) do
    local e, d, cb = unpack(c)
    vim.api.nvim_create_autocmd(e, { desc = d, callback = cb })
  end
end

create_autocmds({
  {
    "FileType",
    "Load EditorConfig settings with highest priority",
    function(ev)
      if vim.F.if_nil(vim.b.editorconfig, vim.g.editorconfig, true) then
        local ok, ec = pcall(require, "editorconfig")
        if ok then
          ec.config(ev.buf)
        end
      end
    end
  },
  {
    "BufWritePre",
    "Create missing parent directories before saving",
    function(ev)
      local is_valid = vim.api.nvim_buf_is_valid(ev.buf) and vim.bo[ev.buf].buflisted
      if is_valid then
        local path = vim.uv.fs_realpath(ev.match) or ev.match
        vim.fn.mkdir(vim.fn.fnamemodify(path, ":p:h"), "p")
      end
    end
  },
  {
    { "InsertLeave", "WinEnter" },
    "Show cursor line in current active window",
    function(ev)
      if vim.bo[ev.buf].buftype == "" then
        vim.opt_local.cursorline = true
      end
    end
  },
  {
    { "InsertEnter", "WinLeave" },
    "Hide cursor line in inactive windows",
    function()
      vim.opt_local.cursorline = false
    end
  },
  {
    "VimResized",
    "Equalize split sizes on resize event",
    function()
      local current_tab = vim.api.nvim_get_current_tabpage()
      vim.cmd.tabdo("wincmd =")
      vim.api.nvim_set_current_tabpage(current_tab)
    end
  },
  {
    { "VimEnter", "VimResume", "ColorScheme" },
    "Sync terminal background color with OSC",
    function()
      local bg_color = vim.api.nvim_get_hl(0, { name = "Normal" }).bg
      io.stdout:write(string.format("\027]11;#%06x\007", bg_color))
    end
  },
  {
    { "VimLeavePre", "VimSuspend" },
    "Revert terminal background color with OSC",
    function()
      io.stdout:write("\027]111;;\007")
    end
  }
})
