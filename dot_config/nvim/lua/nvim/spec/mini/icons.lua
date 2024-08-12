---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
  "mini.icons", dev = true, event = "VeryLazy",

  config = function()
    local icons = require ("mini.icons")
    icons.setup()
    icons.mock_nvim_web_devicons()
  end
}

return Spec
