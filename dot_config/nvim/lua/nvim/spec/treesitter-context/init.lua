---@module "nvim-treesitter-context"

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter-context",

  ---@type TSContext.Config
  opts = {
    max_lines = 1,
    multiline_threshold = 1,
  },
}

return Spec
