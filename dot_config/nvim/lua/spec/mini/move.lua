---@module "mini.move"

---@type LazyPluginSpec
local Spec = {
  "mini.move", virtual = true, opts = {},

  keys = {
    { "<A-h>", mode = { "x" }, desc = "Move left" },
    { "<A-j>", mode = { "x" }, desc = "Move down" },
    { "<A-k>", mode = { "x" }, desc = "Move up" },
    { "<A-l>", mode = { "x" }, desc = "Move right" },
    { "<A-h>", mode = { "n" }, desc = "Move line left" },
    { "<A-j>", mode = { "n" }, desc = "Move line down" },
    { "<A-k>", mode = { "n" }, desc = "Move line up" },
    { "<A-l>", mode = { "n" }, desc = "Move line right" },
  },
}

return Spec
