---@type LazyPluginSpec
return {
  [[nvim-treesitter/nvim-treesitter]],
  name = [[treesitter]],
  event = [[VeryLazy]],

  opts = {
    ensure_installed = [[all]],
    indent = {
      enable = false
    },
    highlight = {
      enable = false
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
