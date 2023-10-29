---@type LazyPluginSpec
return {
  [[nvim-treesitter/nvim-treesitter]],
  name = [[treesitter]],

  opts = {
    auto_install = true,
    ensure_installed = {
      [[markdown]],
      [[markdown_inline]]
    },
    indent = {
      enable = false
    },
    highlight = {
      enable = false
    }
  },

  init = function(plugin)
    ---PERF: Some plugins load treesitter when they only
    ---require query predicates.
    ---
    require("lazy.core.loader").add_to_rtp(plugin)
    require("nvim-treesitter.query_predicates")
  end,

  config = function(_, opts)
    require [[nvim-treesitter.configs]].setup(opts)
  end
}
