---@type LazyPluginSpec
return {
  [[nvim-treesitter/nvim-treesitter]],
  event = [[VeryLazy]],

  opts = {
    ensured_installed = [[all]],
    indent = {
      enable = true
    },
    highlight = {
      enable = true
    }
  },

  config = function(_, opts)
    require("nvim-treesitter.configs").setup(opts)
  end
}
