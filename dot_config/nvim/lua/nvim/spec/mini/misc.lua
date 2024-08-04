-- Source: 'runtime/lua/vim/_defaults.lua' in Neovim source
local parse_osc11 = function (x)
  local r, g, b = x:match  "^\027%]11;rgb:(%x+)/(%x+)/(%x+)$"
  if not (r and g and b) then
    local a
    r, g, b, a = x:match  "^\027%]11;rgba:(%x+)/(%x+)/(%x+)/(%x+)$"
    if not (a and a:len () <= 4) then return end
  end
  if not (r and g and b) then return end
  if not (r:len () <= 4 and g:len () <= 4 and b:len () <= 4) then return end
  local parse_osc_hex = function (c)
    return c:len () == 1 and (c .. c) or c:sub (1, 2)
  end
  return "#" .. parse_osc_hex (r) .. parse_osc_hex (g) .. parse_osc_hex (b)
end

--- Set up terminal background synchronization
---
--- What it does:
--- - Checks if terminal emulator supports OSC 11 control sequence. Stops if not.
--- - Creates |UIEnter| and |ColorScheme| autocommands which change terminal
---   background to have same color as |guibg| of |hl-Normal|.
--- - Creates |UILeave| autocommand which sets terminal background back to the
---   color at the time this function was called first time in current session.
--- - Synchronizes background immediately to allow not depend on loading order.
---
--- Primary use case is to remove possible "frame" around current Neovim instance
--- which appears if Neovim's |hl-Normal| background color differs from what is
--- used by terminal emulator itself.
local setup_termbg_sync = function ()
  local augroup = vim.api.nvim_create_augroup ("MiniMiscTermbgSync", { clear = true })
  local f = function (args)
    local ok, bg_init = pcall (parse_osc11, args.data)
    if not (ok and type (bg_init) == "string") then
      H.notify ("`setup_termbg_sync()` could not parse terminal emulator response " .. vim.inspect (args.data), "WARN")
      return
    end

    -- Set up sync
    local sync = function ()
      local normal = vim.api.nvim_get_hl(0, { name = [[Normal]] })
      if normal.bg == nil then return end
      io.write (string.format ("\027]10;1;#%06x\a", normal.bg))
    end
    vim.api.nvim_create_autocmd ({ "VimEnter", "VimResume", "ColorScheme" }, { group = augroup, callback = sync })
    vim.api.nvim_create_autocmd ("User", { pattern = { "LumenDark", "LumenLight"}, group = augroup, callback = sync })

    -- Set up reset to the color returned from the very first call
    local reset = function () io.write ("\027]111;;\a") end
    vim.api.nvim_create_autocmd ({ "VimLeavePre", "VimSuspend" }, { group = augroup, callback = reset })

    -- Sync immediately
    sync ()
  end

  -- Ask about current background color and process the response
  local id = vim.api.nvim_create_autocmd ("TermResponse", { group = augroup, callback = f, once = true, nested = true })
  io.write "\027]11;?\007"
  vim.defer_fn (function ()
    local ok = pcall (vim.api.nvim_del_autocmd, id)
    if ok then
      H.notify ("`setup_termbg_sync()` did not get response from terminal emulator", "WARN")
    end
  end, 1000)
end

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, lazy = false, priority = 1000,

  config = function()
    local misc = require ("mini.misc")

    -- https://github.com/echasnovski/mini.nvim/issues/1111
    misc.setup_termbg_sync = setup_termbg_sync

    misc.setup_auto_root()
    misc.setup_restore_cursor()
    misc.setup_termbg_sync()
  end
}

return Spec
