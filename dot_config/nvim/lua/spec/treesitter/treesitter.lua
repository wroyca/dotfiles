---@type LazySpec
return {
  [[nvim-treesitter/nvim-treesitter]],
  name = [[treesitter]],
  build = [[:TSInstall all]],

  init = function(plugin)
    require [[lazy.core.loader]].add_to_rtp(plugin)
    require [[nvim-treesitter.query_predicates]]
  end,

  opts = {
    indent = {
      enable = false
    },
    highlight = {
      enable = false
    }
  },

  config = function(_, opts)
    require [[nvim-treesitter.configs]].setup(opts)
  end
}
