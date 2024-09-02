---@module "vim-matchup"

---@type LazyPluginSpec
local Spec = {
  "andymass/vim-matchup", event = "VeryLazy",

  opts = {
    matchparen_offscreen = {},
    matchparen_deffered = 1,
  },

  config = function(_, opts)
    vim.g.matchup_matchparen_offscreen = opts.matchparen_offscreen
    vim.g.matchup_matchparen_deferred = opts.matchparen_deffered
  end
}

return Spec
