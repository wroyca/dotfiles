---@module "mini.operators"

---@type LazyPluginSpec
local Spec = {
  "mini.operators", virtual = true, opts = {},

  keys = {
    { "gm", mode = { "n", "x" } },
  },
}

return Spec
