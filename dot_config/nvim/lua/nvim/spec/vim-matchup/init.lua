---@module "vim-matchup"

---@type LazyPluginSpec
local Spec = {
  "andymass/vim-matchup", event = "VeryLazy",
  opts = {
    matchparen_offscreen = {}
  },
  config = function(_, opts)
    vim.g.matchup_matchparen_offscreen = opts.matchparen_offscreen
  end
}

return Spec
