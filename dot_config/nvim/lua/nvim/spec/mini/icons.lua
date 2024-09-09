---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
  "mini.icons", dev = true,

  config = function()
    local icons = require ("mini.icons")
    icons.setup()
    icons.mock_nvim_web_devicons()
  end
}

return Spec
