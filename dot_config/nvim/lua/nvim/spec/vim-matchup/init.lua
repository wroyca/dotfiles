---@module "vim-matchup"

---@type LazyPluginSpec
local Spec = {
  "andymass/vim-matchup", event = "VeryLazy",
  config = function(_, opts)
    vim.g.matchup_matchparen_offscreen = {""}
  end
}

return Spec
