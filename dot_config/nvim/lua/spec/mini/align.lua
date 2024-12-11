---@module "mini.align"

---@type LazyPluginSpec
local Spec = {
  "mini.align", virtual = true, opts = {},

  keys = {
    { "gA", mode = { "n", "x" }, desc = "Align with preview" },
    { "ga", mode = { "n", "x" }, desc = "Align" },
  },
}

return Spec
