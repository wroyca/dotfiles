---@module "leap"

---@LazyPluginSpec
local Spec = {
  "ggandor/leap.nvim", keys = { "s", "S", "gs" },

  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },

  config = function(_, opts)
    local leap = require("leap")
    leap.setup(opts)
    leap.add_default_mappings(true)
  end,
}

return Spec
