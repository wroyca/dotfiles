---@module "mini.move"

---@type LazyPluginSpec
local Spec = {
  "mini.move", dev = true, opts = {},

  keys = {
    { "<A-h>", mode = { "n", "x" } },
    { "<A-j>", mode = { "n", "x" } },
    { "<A-k>", mode = { "n", "x" } },
    { "<A-l>", mode = { "n", "x" } },
  },
}

return Spec
