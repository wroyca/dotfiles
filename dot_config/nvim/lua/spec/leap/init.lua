---@module "leap"

---@type LazyPluginSpec
local Spec = {
  "ggandor/leap.nvim",

  keys = {
    {
      "s",
      "<Plug>(leap-forward)",
      mode = { "n", "x", "o" },
      desc = "Leap forward",
    },
    {
      "S",
      "<Plug>(leap-backward)",
      mode = { "n", "x", "o" },
      desc = "Leap backward",
    },
  },

  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },
}

return Spec
