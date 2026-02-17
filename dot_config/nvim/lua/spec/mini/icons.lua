---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
  "mini.icons", virtual = true, event = "VeryLazy",

  config = function (_, opts)
    require ("mini.icons").setup (opts)
  end,
}

return Spec
