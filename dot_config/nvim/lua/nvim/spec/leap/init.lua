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

    {
      "<C-s>",
      "<Plug>(leap-from-window)",
      mode = { "n", "x", "o" },
      desc = "Leap from window",
    },

    {
      "r",
      function()
        require("leap.remote").action()
      end,
      mode = { "o" },
      desc = "Leap remote",
    },

    {
      "<C-Space>",
      function()
        require("leap.treesitter").select()
      end,
      mode = { "n", "x", "o" },
      desc = "Leap incremental selection",
    },
  },

  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },
}

return Spec
