---@module "nvim-treesitter-context"
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter-context", event = "VeryLazy",

  ---@type TSContext.Config
  opts = {
    max_lines = 1,
    multiline_threshold = 1
  }
}

return Spec
