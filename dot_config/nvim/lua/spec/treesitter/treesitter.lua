---@type LazySpec
return {
  [[treesitter]],
  event = [[VeryLazy]],
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
      enable = true
    }
  },

  config = function(_, opts)
    require [[nvim-treesitter.configs]].setup(opts)
  end
}
