---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
  "mini.icons", dev = true,

  config = function(_, opts)
    local icons = require ("mini.icons")
    icons.setup(opts)
    icons.mock_nvim_web_devicons()
  end
}

return Spec
