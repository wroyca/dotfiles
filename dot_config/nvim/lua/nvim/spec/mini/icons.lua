---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
  "mini.icons", dev = true,

  config = function(_, opts)
    require("mini.icons").setup(opts)
    MiniIcons.mock_nvim_web_devicons()
  end,
}

return Spec
