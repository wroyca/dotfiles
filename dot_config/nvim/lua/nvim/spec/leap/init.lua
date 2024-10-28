---@module "leap"

---@type LazyPluginSpec
local Spec = {
  "ggandor/leap.nvim",

  -- NOTE: Avoid `create_default_mappings` due to its keymap conflict
  -- detection. It lack context awareness and frequently results in false
  -- positives.
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
