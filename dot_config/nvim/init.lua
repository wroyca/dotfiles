--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd ("lumen")

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

local function create_autocmds (cmds)
  for _, c in ipairs (cmds) do
    local e, d, cb = unpack (c)
    vim.api.nvim_create_autocmd (e, { desc = d, callback = cb })
  end
end

create_autocmds ({
  {
    { "InsertLeave", "WinEnter" },
    "Show cursor line in active window",
    function (ev)
      if vim.bo[ev.buf].buftype == "" then
        vim.opt_local.cursorline = true
      end
    end,
  },
  {
    { "InsertEnter", "WinLeave" },
    "Hide cursor line in inactive windows",
    function ()
      vim.opt_local.cursorline = false
    end,
  },
  {
    { "VimEnter", "VimResume", "ColorScheme" },
    "Sync terminal background color",
    function ()
      if vim.g.colors_name == nil then
        return
      end
      io.stdout:write (string.format ("\027]11;#%06x\007", vim.api.nvim_get_hl (0, { name = "normal" }).bg))
    end,
  },
  {
    { "VimLeavePre", "VimSuspend" },
    "Revert terminal background color",
    function ()
      io.stdout:write ("\027]111;;\007")
    end,
  },
})

local function hi (name, opts)
  ---@diagnostic disable-next-line: deprecated
  local is_ok, hl = pcall (vim.api.nvim_get_hl_by_name, name, true)
  if is_ok then
    vim.iter (opts):each (function (k, v)
      hl[k] = v
    end)
    pcall (vim.api.nvim_set_hl, 0, name, hl)
  end
end

hi ("comment",                      { italic = true                        })
hi ("@comment.error",               { italic = true                        })
hi ("@comment.warning",             { italic = true                        })
hi ("@comment.todo",                { italic = true                        })
hi ("@comment.note",                { italic = true                        })
hi ("todo",                         { italic = true                        })
hi ("minihipatternsfixme",          { italic = true                        })
hi ("minihipatternshack",           { italic = true                        })
hi ("minihipatternsnote",           { italic = true                        })
hi ("minihipatternstodo",           { italic = true                        })
hi ("gitcommitdiscarded",           { italic = false                       })
hi ("gitcommitselected",            { italic = false                       })
hi ("gitcommituntracked",           { italic = false                       })
hi ("diagnosticunnecessary",        { italic = false                       })
hi ("minidepsplaceholder",          { italic = false                       })
hi ("ministarterfooter",            { italic = false                       })
hi ("ministarterinactive",          { italic = false                       })
hi ("lazydimmed",                   { italic = false                       })
hi ("whichkeyvalue",                { italic = false                       })
hi ("dashboardfooter",              { italic = false                       })
hi ("cmpitemabbrdeprecated",        { italic = false, strikethrough = true })
hi ("cocdisabled",                  { italic = false                       })
hi ("cocfadeout",                   { italic = false                       })
hi ("dapuibreakpointsdisabledline", { italic = false                       })
hi ("masonmuted",                   { italic = false                       })
hi ("lspcodelens",                  { italic = false                       })
hi ("lspcodelensseparator",         { italic = false                       })
hi ("leapbackdrop",                 { italic = false                       })
