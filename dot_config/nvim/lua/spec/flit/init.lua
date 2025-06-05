---@module "flit"

---@type LazyPluginSpec[]
local Spec = {
  {
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
  },
  {
    "folke/which-key.nvim",
    opts_extend = { "spec" },
  },
}

-- When Flit is first configured, it overrides our key mapping descriptions. We
-- need to explicitly re-register those mappings with which-key to restore the
-- correct descriptions.

Spec[2].opts = {
  spec = Spec[1].keys,
}

return Spec
