---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, event = "VimEnter",

  config = function()
    local misc = require "mini.misc"
    misc.setup()
    misc.setup_auto_root()
    misc.setup_restore_cursor()

    -- https://github.com/echasnovski/mini.nvim/issues/1128#issuecomment-2282193831
    misc.setup_termbg_sync = (function()
      local sync = function() io.write(string.format("\027]11;#%06x\007", vim.api.nvim_get_hl(0, { name = "Normal" }).bg)) end
      vim.api.nvim_create_autocmd({ "UIEnter", "ColorScheme" }, { callback = sync })
      local reset = function() io.write('\027]111\007') end
      vim.api.nvim_create_autocmd({ 'UILeave' }, { callback = reset })
    end)()
  end
}

return Spec
