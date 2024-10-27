---@module "leap"

---@type LazyPluginSpec
local Spec = {
  "ggandor/leap.nvim",

  keys = {
    "s", "S", "gs",

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

  config = function(_, opts)
    require("leap").setup(opts)
    require("leap").create_default_mappings()
  end,
}

return Spec
