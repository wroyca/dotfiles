---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, event = "VimEnter",

  opts = {
    osc_111 = true
  },

  config = function(_, opts)
    local misc = require "mini.misc"
    misc.setup()
    misc.setup_auto_root()
    misc.setup_restore_cursor()

    -- For compatibility, `setup_termbg_sync()` defaults to using OSC 11 over
    -- 111. That is, it caches the background color and resends it as is in the
    -- sequence. This approach can cause issues with terminals that respect the
    -- OS's dark/light mode preference, potentially resulting in a mismatch
    -- (e.g., sending a "black" background color when the OS is in light mode,
    -- causing an expectation of a white background).
    --
    -- For now, use OSC 111 by default. Consider terminals that do not support
    -- this properly as an upstream bug to be reported to respective terminal
    -- developers, but still maintain the option to fallback to OSC 11 for
    -- users using such obscure terminals.
    --
    -- https://github.com/echasnovski/mini.nvim/issues/1128#issuecomment-2282193831
    --
    -- stylua: ignore start
    if opts.osc_111 then
      ---@diagnostic disable-next-line: duplicate-set-field
      misc.setup_termbg_sync = function()
        local sync = function()
          local hl = vim.api.nvim_get_hl(0, { name = "Normal" })
          if hl and hl.bg then
            io.write(string.format("\027]11;#%06x\007", hl.bg))
          end
        end
        local reset = function() io.write "\027]111\007" end

        vim.api.nvim_create_autocmd({ "UIEnter", "ColorScheme" }, { callback = sync })
        vim.api.nvim_create_autocmd({ "UILeave" }, { callback = reset })
      end
    end
    misc.setup_termbg_sync()
  end
}

return Spec
