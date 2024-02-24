---@type LazySpec
return {
  [[treesitter]],
  event = [[VeryLazy]],
  -- https://github.com/neovim/neovim/pull/22923
  --
  init = function(plugin)
    require([[lazy.core.loader]]).add_to_rtp(plugin)
    require([[nvim-treesitter.query_predicates]])
  end,
  opts = {
    ensure_installed = [[all]],
    indent = {
      enable = false,
    },
    highlight = {
      enable = true,
    },
  },
}
