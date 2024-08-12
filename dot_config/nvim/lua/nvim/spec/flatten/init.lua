---@module "flatten"

---@type LazyPluginSpec
local Spec = {
  "willothy/flatten.nvim",
  opts = {
    one_per = {
      kitty = true,
    }
  },
  lazy = false, priority = 1001
}

return Spec
