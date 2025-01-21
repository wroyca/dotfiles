---@module "mason"

---@type LazyPluginSpec
local Spec = {
  "williamboman/mason.nvim",

  opts = {
    max_concurrent_installers = 10,
    pip = {
      upgrade_pip = true,
    },
  },
}

return Spec
