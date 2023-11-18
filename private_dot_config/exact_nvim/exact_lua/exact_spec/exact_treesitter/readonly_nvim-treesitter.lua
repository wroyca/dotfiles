---@type LazyPluginSpec
return {
  [[nvim-treesitter/nvim-treesitter]],
  name = [[treesitter]],
  build = [[:TSInstall all]],

  opts = {
    indent = {
      enable = false
    },
    highlight = {
      enable = false
    },
    context_commentstring = {
      enable = true
    }
  },

  init = function(plugin)
    require [[lazy.core.loader]].add_to_rtp(plugin)
    require [[nvim-treesitter.query_predicates]]
  end,

  config = function(_, opts)
    require [[nvim-treesitter.configs]].setup(opts)
  end
}
