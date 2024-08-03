---@type LazyPluginSpec
local Spec = {
  "mini.move", dev = true, config = true,

  keys = {
    { "<A-h>", mode = { "n", "x" } },
    { "<A-j>", mode = { "n", "x" } },
    { "<A-k>", mode = { "n", "x" } },
    { "<A-l>", mode = { "n", "x" } },
  },
}

return Spec
