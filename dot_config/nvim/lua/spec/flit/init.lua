---@module "flit.nvim"

---@type LazyPluginSpec
local Spec = {
  "ggandor/flit.nvim", dependencies = "ggandor/leap.nvim",

  keys = {
    { "f", mode = { "n", "x", "o" }, desc = "Flit forward to" },
    { "F", mode = { "n", "x", "o" }, desc = "Flit backward to" },
    { "t", mode = { "n", "x", "o" }, desc = "Flit forward till" },
    { "T", mode = { "n", "x", "o" }, desc = "Flit backward till" },
  },

  opts = {
    multiline = false,
    labeled_modes = "nx",
  },
}

return Spec
